package scala.meta.ls

import scala.concurrent.Future
import scala.meta.internal.bsp.BuildChange
import scala.meta.internal.builds.BuildTool
import scala.meta.internal.metals.Messages
import scala.util.control.NonFatal
import org.eclipse.lsp4j.MessageParams
import org.eclipse.lsp4j.MessageType
import scala.meta.internal.bsp.BspSession
import scala.meta.internal.metals.Tables
import scala.meta.internal.semver.SemVer
import scala.meta.internal.metals.DelegatingLanguageClient
import scala.meta.internal.builds.BuildTools
import scala.meta.internal.bsp.BspConnector
import scala.meta.internal.metals.Warnings
import scala.meta.internal.builds.BuildToolSelector
import scala.concurrent.ExecutionContextExecutorService
import scala.meta.io.AbsolutePath
import scala.meta.internal.builds.BloopInstall
import scala.concurrent.Promise
import scala.meta.internal.metals.Compilations
import scala.meta.internal.metals.Buffers
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.Compilers
import scala.meta.internal.metals.TimerProvider
import scala.meta.internal.metals.UserConfiguration
import scala.meta.internal.tvp.TreeViewProvider
import scala.meta.internal.metals.StatusBar
import scala.meta.internal.metals.Doctor
import scala.meta.internal.metals.ImportedBuild
import scala.meta.internal.metals.BuildTargets
import scala.meta.internal.metals.Diagnostics
import scala.meta.internal.metals.BuildInfo
import scala.meta.internal.metals.BloopServers
import scala.meta.internal.metals.Cancelable
import scala.meta.internal.metals.MutableCancelable
import java.util.concurrent.atomic.AtomicBoolean

final class BuildServerManager(
    tables: Tables,
    languageClient: DelegatingLanguageClient,
    buildTools: BuildTools,
    warnings: Warnings,
    buildToolSelector: BuildToolSelector,
    executionContext: ExecutionContextExecutorService,
    workspace: () => AbsolutePath,
    indexer: Indexer,
    bloopInstall: BloopInstall,
    buildServerPromise: Promise[Unit],
    compilations: Compilations,
    buffers: Buffers,
    compilers: Compilers,
    timerProvider: TimerProvider,
    bspConnector: BspConnector,
    userConfig: () => UserConfiguration,
    treeView: () => TreeViewProvider,
    bspSession: () => Option[BspSession],
    setBspSession: Option[BspSession] => Unit,
    statusBar: StatusBar,
    doctor: Doctor,
    buildTargets: BuildTargets,
    diagnostics: Diagnostics,
    bloopServers: BloopServers
) extends Cancelable {

  private val cancelables = new MutableCancelable()
  private val isCancelled = new AtomicBoolean(false)
  def cancel(): Unit =
    if (isCancelled.compareAndSet(false, true)) {
      try cancelables.cancel()
      catch {
        case NonFatal(_) =>
      }
    }

  private implicit def ec = executionContext

  def supportedBuildTool(): Future[Option[BuildTool]] = {
    def isCompatibleVersion(buildTool: BuildTool) = {
      val isCompatibleVersion = SemVer.isCompatibleVersion(
        buildTool.minimumVersion,
        buildTool.version
      )
      if (isCompatibleVersion) {
        Some(buildTool)
      } else {
        scribe.warn(s"Unsupported $buildTool version ${buildTool.version}")
        languageClient.showMessage(
          Messages.IncompatibleBuildToolVersion.params(buildTool)
        )
        None
      }
    }

    buildTools.loadSupported match {
      case Nil => {
        if (!buildTools.isAutoConnectable) {
          warnings.noBuildTool()
        }
        Future(None)
      }
      case buildTool :: Nil => Future(isCompatibleVersion(buildTool))
      case buildTools =>
        for {
          Some(buildTool) <- buildToolSelector.checkForChosenBuildTool(
            buildTools
          )
        } yield isCompatibleVersion(buildTool)
    }
  }

  def slowConnectToBuildServer(
      forceImport: Boolean
  ): Future[BuildChange] = {
    for {
      possibleBuildTool <- supportedBuildTool()
      chosenBuildServer = tables.buildServers.selectedServer()
      isBloopOrEmpty = chosenBuildServer.isEmpty || chosenBuildServer.exists(
        _ == BspConnector.BLOOP_SELECTED
      )
      buildChange <- possibleBuildTool match {
        case Some(buildTool) =>
          buildTool.digest(workspace()) match {
            case None =>
              scribe.warn(s"Skipping build import, no checksum.")
              Future.successful(BuildChange.None)
            case Some(digest) if isBloopOrEmpty =>
              slowConnectToBloopServer(forceImport, buildTool, digest)
            case Some(digest) =>
              indexer.reloadWorkspaceAndIndex(forceImport, buildTool, digest)
          }
        case None =>
          Future.successful(BuildChange.None)
      }
    } yield buildChange
  }

  def slowConnectToBloopServer(
      forceImport: Boolean,
      buildTool: BuildTool,
      checksum: String
  ): Future[BuildChange] =
    for {
      result <- {
        if (forceImport) bloopInstall.runUnconditionally(buildTool)
        else bloopInstall.runIfApproved(buildTool, checksum)
      }
      change <- {
        if (result.isInstalled) quickConnectToBuildServer()
        else if (result.isFailed) {
          if (buildTools.isAutoConnectable) {
            // TODO(olafur) try to connect but gracefully error
            languageClient.showMessage(
              Messages.ImportProjectPartiallyFailed
            )
            // Connect nevertheless, many build import failures are caused
            // by resolution errors in one weird module while other modules
            // exported successfully.
            quickConnectToBuildServer()
          } else {
            languageClient.showMessage(Messages.ImportProjectFailed)
            Future.successful(BuildChange.Failed)
          }
        } else {
          Future.successful(BuildChange.None)
        }
      }
    } yield change

  def quickConnectToBuildServer(): Future[BuildChange] = {
    val connected = if (!buildTools.isAutoConnectable) {
      scribe.warn("Build server is not auto-connectable.")
      Future.successful(BuildChange.None)
    } else {
      autoConnectToBuildServer()
    }

    connected.map { change =>
      buildServerPromise.trySuccess(())
      change
    }
  }

  def autoConnectToBuildServer(): Future[BuildChange] = {
    def compileAllOpenFiles: BuildChange => Future[BuildChange] = {
      case change if !change.isFailed =>
        Future
          .sequence[Unit, List](
            compilations
              .cascadeCompileFiles(buffers.open.toSeq)
              .ignoreValue ::
              compilers.load(buffers.open.toSeq) ::
              Nil
          )
          .map(_ => change)
      case other => Future.successful(other)
    }

    (for {
      _ <- disconnectOldBuildServer()
      maybeSession <- timerProvider.timed("Connected to build server", true) {
        bspConnector.connect(workspace(), userConfig())
      }
      result <- maybeSession match {
        case Some(session) =>
          val result = connectToNewBuildServer(session)
          session.mainConnection.onReconnection { newMainConn =>
            val updSession = session.copy(main = newMainConn)
            connectToNewBuildServer(updSession)
              .flatMap(compileAllOpenFiles)
              .ignoreValue
          }
          result
        case None =>
          Future.successful(BuildChange.None)
      }
      _ = {
        treeView().init()
      }
    } yield result)
      .recover { case NonFatal(e) =>
        disconnectOldBuildServer()
        val message =
          "Failed to connect with build server, no functionality will work."
        val details = " See logs for more details."
        languageClient.showMessage(
          new MessageParams(MessageType.Error, message + details)
        )
        scribe.error(message, e)
        BuildChange.Failed
      }
      .flatMap(compileAllOpenFiles)
  }

  def disconnectOldBuildServer(): Future[Unit] = {
    bspSession().foreach(connection =>
      scribe.info(s"Disconnecting from ${connection.main.name} session...")
    )

    bspSession() match {
      case None => Future.successful(())
      case Some(session) =>
        setBspSession(None)
        diagnostics.reset()
        buildTargets.resetConnections(List.empty)
        session.shutdown()
    }
  }

  def connectToNewBuildServer(
      session: BspSession
  ): Future[BuildChange] = {
    scribe.info(
      s"Connected to Build server: ${session.main.name} v${session.version}"
    )
    cancelables.add(session)
    compilers.cancel()
    setBspSession(Some(session))
    val importedBuilds0 = timerProvider.timed("Imported build") {
      session.importBuilds()
    }
    for {
      bspBuilds <- statusBar.trackFuture("Importing build", importedBuilds0)
      _ = {
        val idToConnection = bspBuilds.flatMap { bspBuild =>
          val targets =
            bspBuild.build.workspaceBuildTargets.getTargets().asScala
          targets.map(t => (t.getId(), bspBuild.connection))
        }
        buildTargets.resetConnections(idToConnection)
        lastImportedBuilds0 = bspBuilds.map(_.build)
      }
      _ <- indexer.profiledIndexWorkspace(() => doctor.check())
      _ = if (session.main.isBloop) checkRunningBloopVersion(session.version)
    } yield {
      BuildChange.Reconnected
    }
  }

  private var lastImportedBuilds0 = List.empty[ImportedBuild]
  def lastImportedBuilds: List[ImportedBuild] = lastImportedBuilds0

  private def checkRunningBloopVersion(bspServerVersion: String) = {
    if (doctor.isUnsupportedBloopVersion()) {
      val notification = tables.dismissedNotifications.IncompatibleBloop
      if (!notification.isDismissed) {
        val messageParams = Messages.IncompatibleBloopVersion.params(
          bspServerVersion,
          BuildInfo.bloopVersion,
          isChangedInSettings = userConfig().bloopVersion != None
        )
        languageClient.showMessageRequest(messageParams).asScala.foreach {
          case action if action == Messages.IncompatibleBloopVersion.shutdown =>
            bloopServers.shutdownServer()
            autoConnectToBuildServer()
          case action
              if action == Messages.IncompatibleBloopVersion.dismissForever =>
            notification.dismissForever()
          case _ =>
        }
      }
    }
  }

}
