package scala.meta.internal.metals

import java.io.IOException
import java.io.InputStream
import java.net.URI
import java.util.Collections
import java.util.concurrent.CompletableFuture
import java.util.concurrent.TimeUnit
import java.util.concurrent.TimeoutException
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicReference

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContextExecutorService
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.reflect.ClassTag
import scala.util.Try

import scala.meta.internal.builds.SbtBuildTool
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.pc.InterruptException
import scala.meta.io.AbsolutePath

import ch.epfl.scala.bsp4j._
import com.google.gson.Gson
import org.eclipse.lsp4j.jsonrpc.JsonRpcException
import org.eclipse.lsp4j.jsonrpc.Launcher
import org.eclipse.lsp4j.services.LanguageClient
import scala.meta.ls.MetalsThreads

/**
 * An actively running and initialized BSP connection.
 */
class BuildServerConnection private (
    reestablishConnection: () => Future[
      BuildServerConnection.LauncherConnection
    ],
    initialConnection: BuildServerConnection.LauncherConnection,
    languageClient: LanguageClient,
    reconnectNotification: DismissedNotifications#Notification,
    config: MetalsServerConfig,
    workspace: AbsolutePath,
    threads: MetalsThreads
)(ec: ExecutionContextExecutorService)
    extends Cancelable {

  @volatile private var connection = Future.successful(initialConnection)
  initialConnection.onConnectionFinished(reconnect)(threads.dummyEc) // !!!

  private val isShuttingDown = new AtomicBoolean(false)
  private val onReconnection =
    new AtomicReference[BuildServerConnection => Future[Unit]](_ =>
      Future.successful(())
    )

  private val _version = new AtomicReference(initialConnection.version)

  private val ongoingRequests =
    new MutableCancelable().addAll(initialConnection.cancelables)

  def version: String = _version.get()

  // the name is set before when establishing conenction
  def name: String = initialConnection.socketConnection.serverName

  def isBloop: Boolean = name == BloopServers.name

  def isSbt: Boolean = name == SbtBuildTool.name

  // hasDebug is not yet available in BSP capabilities
  // https://github.com/build-server-protocol/build-server-protocol/pull/161
  def hasDebug: Boolean = isBloop || isSbt

  def workspaceDirectory: AbsolutePath = workspace

  def onReconnection(
      index: BuildServerConnection => Future[Unit]
  ): Unit = {
    onReconnection.set(index)
  }

  /**
   * Run build/shutdown procedure
   */
  def shutdown(): Future[Unit] =
    connection.map { conn =>
      try {
        if (isShuttingDown.compareAndSet(false, true)) {
          conn.server.buildShutdown().get(2, TimeUnit.SECONDS)
          conn.server.onBuildExit()
          scribe.info("Shut down connection with build server.")
          // Cancel pending compilations on our side, this is not needed for Bloop.
          cancel()
        }
      } catch {
        case _: TimeoutException =>
          scribe.error(
            s"timeout: build server '${conn.displayName}' during shutdown"
          )
        case InterruptException() =>
        case e: Throwable =>
          scribe.error(
            s"build shutdown: ${conn.displayName}",
            e
          )
      }
    }(threads.buildServerConnStuffEc)

  def compile(params: CompileParams): CompletableFuture[CompileResult] = {
    register(server => server.buildTargetCompile(params))
  }

  def clean(params: CleanCacheParams): CompletableFuture[CleanCacheResult] = {
    register(server => server.buildTargetCleanCache(params))
  }

  def workspaceReload(): Future[Object] = {
    if (initialConnection.capabilities.getCanReload()) {
      register(server => server.workspaceReload()).asScala
    } else {
      scribe.warn(
        s"${initialConnection.displayName} does not support `workspace/reload`, unable to reload"
      )
      Future.successful(null)
    }
  }

  def mainClasses(
      params: ScalaMainClassesParams
  ): Future[ScalaMainClassesResult] = {
    register(server => server.buildTargetScalaMainClasses(params)).asScala
  }

  def testClasses(
      params: ScalaTestClassesParams
  ): Future[ScalaTestClassesResult] = {
    register(server => server.buildTargetScalaTestClasses(params)).asScala
  }

  def startDebugSession(params: DebugSessionParams): Future[URI] = {
    register(server => server.startDebugSession(params)).asScala
      .map(address => URI.create(address.getUri))(threads.dummyEc)
  }

  def workspaceBuildTargets(): Future[WorkspaceBuildTargetsResult] = {
    register(server => server.workspaceBuildTargets()).asScala
  }

  def buildTargetScalacOptions(
      params: ScalacOptionsParams
  ): Future[ScalacOptionsResult] = {
    register(server => server.buildTargetScalacOptions(params)).asScala
  }

  def buildTargetSources(params: SourcesParams): Future[SourcesResult] = {
    register(server => server.buildTargetSources(params)).asScala
  }

  def buildTargetDependencySources(
      params: DependencySourcesParams
  ): Future[DependencySourcesResult] = {
    register(server => server.buildTargetDependencySources(params)).asScala
  }

  private val cancelled = new AtomicBoolean(false)

  override def cancel(): Unit = {
    if (cancelled.compareAndSet(false, true)) {
      ongoingRequests.cancel()
    }
  }

  private def askUser(): Future[BuildServerConnection.LauncherConnection] = {
    if (config.askToReconnect) {
      if (!reconnectNotification.isDismissed) {
        val params = Messages.DisconnectedServer.params()
        languageClient
          .showMessageRequest(params)
          .asScala
          .flatMap {
            case response
                if response == Messages.DisconnectedServer.reconnect =>
              reestablishConnection()
            case response if response == Messages.DisconnectedServer.notNow =>
              Future(reconnectNotification.dismiss(5, TimeUnit.MINUTES))(
                threads.dbEc
              )
                .flatMap(_ => connection)(threads.dummyEc)
            case _ =>
              connection
          }(threads.dummyEc)
      } else {
        connection
      }
    } else {
      reestablishConnection()
    }
  }

  private def reconnect(): Future[BuildServerConnection.LauncherConnection] = {
    val original = connection
    if (!isShuttingDown.get()) {
      synchronized {
        // if the future is different then the connection is already being reestablished
        if (connection eq original) {
          connection = askUser().map { conn =>
            // version can change when reconnecting
            _version.set(conn.version)
            ongoingRequests.addAll(conn.cancelables)
            conn.onConnectionFinished(reconnect)(threads.dummyEc) // !!!
            conn
          }(threads.dummyEc)
          connection
            .foreach(_ => onReconnection.get()(this))(threads.dummyEc) // !!!
        }
        connection
      }
    } else {
      connection
    }

  }
  private def register[T: ClassTag](
      action: MetalsBuildServer => CompletableFuture[T]
  ): CompletableFuture[T] = {
    val original = connection
    val actionFuture = original
      .flatMap { launcherConnection =>
        val resultFuture = action(launcherConnection.server)
        ongoingRequests.add(
          Cancelable(() =>
            Try(resultFuture.completeExceptionally(new InterruptedException()))
          )
        )
        resultFuture.asScala
      }(threads.dummyEc)
      .recoverWith {
        case io: JsonRpcException if io.getCause.isInstanceOf[IOException] =>
          // remove a synchronized here…
          reconnect()
            .flatMap(conn => action(conn.server).asScala)(threads.dummyEc)
        case t
            if implicitly[ClassTag[T]].runtimeClass.getSimpleName != "Object" =>
          val name = implicitly[ClassTag[T]].runtimeClass.getSimpleName
          Future.failed(MetalsBspException(name, t.getMessage))
      }(threads.dummyEc)
    CancelTokens.future(_ => actionFuture)(ec)
  }

}

object BuildServerConnection {

  /**
   * Establishes a new build server connection with the given input/output streams.
   *
   * This method is blocking, doesn't return Future[], because if the `initialize` handshake
   * doesn't complete within a few seconds then something is wrong. We want to fail fast
   * when initialization is not successful.
   */
  def fromSockets(
      workspace: AbsolutePath,
      localClient: MetalsBuildClient,
      languageClient: LanguageClient,
      connect: () => Future[SocketConnection],
      reconnectNotification: DismissedNotifications#Notification,
      config: MetalsServerConfig,
      serverName: String,
      threads: MetalsThreads,
      retry: Int = 5
  ): Future[BuildServerConnection] = {

    def setupServer(): Future[LauncherConnection] = {
      connect().map { case conn @ SocketConnection(_, output, input, _, _) =>
        val tracePrinter = GlobalTrace.setupTracePrinter("BSP")
        val launcher = new Launcher.Builder[MetalsBuildServer]()
          .traceMessages(tracePrinter)
          .setOutput(output)
          .setInput(input)
          .setLocalService(localClient)
          .setRemoteInterface(classOf[MetalsBuildServer])
          .setExecutorService(threads.buildServerJsonRpcEs)
          .create()
        val listening = launcher.startListening()
        val server = launcher.getRemoteProxy
        val stopListening =
          Cancelable(() => listening.cancel(false))
        val result =
          try {
            BuildServerConnection.initialize(workspace, server, serverName)
          } catch {
            case e: TimeoutException =>
              conn.cancelables.foreach(_.cancel())
              stopListening.cancel()
              scribe.error("Timeout waiting for 'build/initialize' response")
              throw e
          }
        LauncherConnection(
          conn,
          server,
          result.getDisplayName(),
          stopListening,
          result.getVersion(),
          result.getCapabilities()
        )
      }(threads.tempEc)
    }

    setupServer()
      .map { connection =>
        new BuildServerConnection(
          setupServer,
          connection,
          languageClient,
          reconnectNotification,
          config,
          workspace,
          threads
        )(threads.tempEcs)
      }(threads.tempEc)
      .recoverWith { case e: TimeoutException =>
        if (retry > 0) {
          scribe.warn(s"Retrying connection to the build server $serverName")
          fromSockets(
            workspace,
            localClient,
            languageClient,
            connect,
            reconnectNotification,
            config,
            serverName,
            threads,
            retry - 1
          )
        } else {
          Future.failed(e)
        }
      }(threads.tempEc)
  }

  final case class BspExtraBuildParams(
      semanticdbVersion: String,
      supportedScalaVersions: java.util.List[String]
  )

  /**
   * Run build/initialize handshake
   */
  private def initialize(
      workspace: AbsolutePath,
      server: MetalsBuildServer,
      serverName: String
  ): InitializeBuildResult = {
    val extraParams = BspExtraBuildParams(
      BuildInfo.scalametaVersion,
      BuildInfo.supportedScala2Versions.asJava
    )

    val initializeResult = server.buildInitialize {
      val params = new InitializeBuildParams(
        "Metals",
        BuildInfo.metalsVersion,
        BuildInfo.bspVersion,
        workspace.toURI.toString,
        new BuildClientCapabilities(
          Collections.singletonList("scala")
        )
      )
      val gson = new Gson
      val data = gson.toJsonTree(extraParams)
      params.setData(data)
      params
    }
    // Block on the `build/initialize` request because it should respond instantly
    // and we want to fail fast if the connection is not
    val result =
      if (serverName == SbtBuildTool.name) {
        initializeResult.get(60, TimeUnit.SECONDS)
      } else {
        initializeResult.get(20, TimeUnit.SECONDS)
      }

    server.onBuildInitialized()
    result
  }

  private case class LauncherConnection(
      socketConnection: SocketConnection,
      server: MetalsBuildServer,
      displayName: String,
      cancelServer: Cancelable,
      version: String,
      capabilities: BuildServerCapabilities
  ) {

    def cancelables: List[Cancelable] =
      cancelServer :: socketConnection.cancelables

    def onConnectionFinished(
        f: () => Unit
    )(ec: ExecutionContext): Unit = {
      socketConnection.finishedPromise.future.foreach(_ => f())(ec)
    }
  }
}

case class SocketConnection(
    serverName: String,
    output: ClosableOutputStream,
    input: InputStream,
    cancelables: List[Cancelable],
    finishedPromise: Promise[Unit]
)
