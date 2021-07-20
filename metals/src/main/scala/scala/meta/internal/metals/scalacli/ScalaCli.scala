package scala.meta.internal.metals.scalacli

import java.io.File
import java.io.InputStream
import java.net.URI
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.util.NoSuchElementException
import java.util.Scanner
import java.util.concurrent.CompletableFuture
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.util.Failure
import scala.util.Properties
import scala.util.Success
import scala.util.control.NonFatal

import scala.meta.internal.bsp.BuildChange
import scala.meta.internal.metals.Buffers
import scala.meta.internal.metals.BuildServerConnection
import scala.meta.internal.metals.BuildTargets
import scala.meta.internal.metals.Cancelable
import scala.meta.internal.metals.ClosableOutputStream
import scala.meta.internal.metals.Compilations
import scala.meta.internal.metals.Compilers
import scala.meta.internal.metals.Diagnostics
import scala.meta.internal.metals.ImportedBuild
import scala.meta.internal.metals.MetalsBuildClient
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.MetalsServerConfig
import scala.meta.internal.metals.MutableCancelable
import scala.meta.internal.metals.SocketConnection
import scala.meta.internal.metals.StatusBar
import scala.meta.internal.metals.Tables
import scala.meta.io.AbsolutePath
import scala.meta.ls.MetalsThreads

import org.eclipse.lsp4j.services.LanguageClient

class ScalaCli(
    compilers: Compilers,
    compilations: Compilations,
    statusBar: StatusBar,
    buffers: Buffers,
    indexWorkspace: () => Future[Unit],
    diagnostics: Diagnostics,
    workspace: () => AbsolutePath,
    tables: Tables,
    threads: MetalsThreads,
    buildClient: MetalsBuildClient,
    languageClient: LanguageClient,
    config: () => MetalsServerConfig,
    ec: ExecutionContext
) extends Cancelable {

  private val cancelables = new MutableCancelable
  private val isCancelled = new AtomicBoolean(false)
  def cancel(): Unit =
    if (isCancelled.compareAndSet(false, true))
      try cancelables.cancel()
      catch {
        case NonFatal(_) =>
      }

  private var buildServer0 = Option.empty[BuildServerConnection]
  private var lastImportedBuild0 = ImportedBuild.empty
  private var roots0 = Seq.empty[AbsolutePath]

  private def allSources(): Seq[AbsolutePath] = {
    val sourceItems = lastImportedBuild0.sources.getItems.asScala.toVector
    val sources = sourceItems.flatMap(_.getSources.asScala.map(_.getUri))
    val roots = sourceItems
      .flatMap(item => Option(item.getRoots).toSeq)
      .flatMap(_.asScala)
    (sources ++ roots).map(new URI(_)).map(AbsolutePath.fromAbsoluteUri(_))
  }

  val buildTargetsData = BuildTargets.Data.create()
  def lastImportedBuild: ImportedBuild =
    lastImportedBuild0
  def buildServer: Option[BuildServerConnection] =
    buildServer0
  def roots: Seq[AbsolutePath] =
    roots0

  def importBuild(): Future[Unit] =
    buildServer0 match {
      case None =>
        Future.failed(new Exception("No Scala CLI server running"))
      case Some(conn) =>
        compilers.cancel()

        implicit val ec0 = ec
        for {
          build0 <- statusBar.trackFuture(
            "Importing Scala CLI sources",
            ImportedBuild.fromConnection(conn)
          )
          _ = {
            lastImportedBuild0 = build0
            val targets = build0.workspaceBuildTargets.getTargets.asScala
            val connections =
              targets.iterator.map(_.getId).map((_, conn)).toList
            buildTargetsData.resetConnections(connections)
            // buildTargetsData.addMappedSource(???, mapped)
          }
          _ <- indexWorkspace()
          allSources0 = allSources()
          toCompile = buffers.open.toSeq.filter(p =>
            allSources0.exists(root =>
              root == p || p.toNIO.startsWith(root.toNIO)
            )
          )
          _ <- Future.sequence[Unit, List](
            compilations
              .cascadeCompileFiles(toCompile) ::
              compilers.load(toCompile) ::
              Nil
          )
        } yield ()
    }

  private def disconnectOldBuildServer(): Future[Unit] = {
    if (buildServer0.isDefined)
      scribe.info("disconnected: Scala CLI server")
    buildServer0 match {
      case None => Future.unit
      case Some(value) =>
        val allSources0 = allSources()
        buildServer0 = None
        lastImportedBuild0 = ImportedBuild.empty
        roots0 = Nil
        cancelables.cancel()
        diagnostics.reset(allSources0)
        value.shutdown()
    }
  }

  private lazy val baseCommand = {

    def endsWithCaseInsensitive(s: String, suffix: String): Boolean =
      s.length >= suffix.length &&
        s.regionMatches(
          true,
          s.length - suffix.length,
          suffix,
          0,
          suffix.length
        )

    def findInPath(app: String): Option[Path] = {
      val asIs = Paths.get(app)
      if (Paths.get(app).getNameCount >= 2) Some(asIs)
      else {
        def pathEntries =
          Option(System.getenv("PATH")).iterator
            .flatMap(_.split(File.pathSeparator).iterator)
        def pathSep =
          if (Properties.isWin)
            Option(System.getenv("PATHEXT")).iterator
              .flatMap(_.split(File.pathSeparator).iterator)
          else Iterator("")
        def matches = for {
          dir <- pathEntries
          ext <- pathSep
          app0 = if (endsWithCaseInsensitive(app, ext)) app else app + ext
          path = Paths.get(dir).resolve(app0)
          if Files.isExecutable(path)
        } yield path
        matches.toStream.headOption
      }
    }

    val cliCommand = sys.props
      .get("metals.scala-cli.launcher")
      .orElse(findInPath("scala-cli").map(_.toString))
      .orElse(findInPath("scala").map(_.toString))
      .getOrElse {
        scribe.warn("scala-cli or scala not found in PATH")
        "scala"
      }
    Seq(cliCommand, "bsp", "-v", "-v", "-v")
  }

  def start(roots: Seq[AbsolutePath]): Future[Unit] = {

    disconnectOldBuildServer().onComplete({
      case Failure(e) =>
        scribe.warn("Error disconnecting old Scala CLI server", e)
      case Success(()) =>
    })(ec)

    val command =
      baseCommand ++ Seq(roots.head.toString) ++ roots.tail.flatMap(p =>
        Seq(";", p.toString)
      )

    val futureConn = BuildServerConnection.fromSockets(
      workspace(),
      buildClient,
      languageClient,
      () => ScalaCli.socketConn(command, workspace())(ec),
      tables.dismissedNotifications.ReconnectScalaCli,
      config(),
      "Scala CLI",
      threads
    )

    implicit val ec0 = ec
    val f = for {
      conn <- futureConn
      _ <- connectToNewBuildServer(conn, roots)
    } yield ()

    f.transform {
      case Failure(ex) =>
        scribe.error("Error starting Scala CLI", ex)
        Success(())
      case Success(other) =>
        scribe.info("Scala CLI started")
        Success(other)
    }
  }

  def stop(): CompletableFuture[Object] =
    disconnectOldBuildServer().asJavaObject

  private def connectToNewBuildServer(
      build: BuildServerConnection,
      roots: Seq[AbsolutePath]
  ): Future[BuildChange] = {
    scribe.info(s"Connected to Scala CLI server v${build.version}")
    cancelables.add(build)
    buildServer0 = Some(build)
    roots0 = roots
    implicit val ec0 = ec
    for {
      _ <- importBuild()
    } yield BuildChange.Reconnected
  }

}

object ScalaCli {

  private def socketConn(
      command: Seq[String],
      workspace: AbsolutePath
  )(implicit ec: ExecutionContext): Future[SocketConnection] =
    // meh, blocks on random ec
    Future {
      scribe.info(s"Running $command")
      val b = new ProcessBuilder(command: _*)
        .redirectInput(ProcessBuilder.Redirect.PIPE)
        .redirectOutput(ProcessBuilder.Redirect.PIPE)
        .redirectError(ProcessBuilder.Redirect.PIPE)
        .directory(workspace.toFile)
      val proc = b.start()
      val os = new ClosableOutputStream(proc.getOutputStream, "Scala CLI")
      var stopSendingOutput = false
      val sendOutput = logOutputThread(proc.getErrorStream, stopSendingOutput)
      sendOutput.start()
      val finished = Promise[Unit]()
      Future {
        proc.waitFor()
        finished.success(())
        ()
      }(ec).onComplete {
        case Success(()) =>
        case f @ Failure(_) => finished.tryComplete(f)
      }(ec)
      SocketConnection(
        "Ammonite",
        os,
        proc.getInputStream,
        List(
          Cancelable { () => proc.destroyForcibly() },
          Cancelable { () => stopSendingOutput = true }
        ),
        finished
      )
    }(ec)

  private val threadCounter = new AtomicInteger
  private def logOutputThread(
      is: InputStream,
      stopSendingOutput: => Boolean
  ): Thread =
    new Thread(
      s"scala-cli-bsp-stderr-to-metals-log-${threadCounter.incrementAndGet()}"
    ) {
      setDaemon(true)
      override def run(): Unit = {
        val reader = new Scanner(is)
        var line: String = null
        while (
          !stopSendingOutput && {
            line =
              try reader.nextLine()
              catch {
                case _: NoSuchElementException =>
                  null
              }
            line != null
          }
        )
          scribe.info("Scala CLI: " + line)
      }
    }

}
