package scala.meta.metals

import java.nio.charset.StandardCharsets
import java.util.concurrent.Executors

import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal

import scala.meta.internal.metals.BuildInfo
import scala.meta.internal.metals.GlobalTrace
import scala.meta.internal.metals.MetalsLanguageClient
import scala.meta.internal.metals.MetalsLanguageServer
import scala.meta.internal.metals.MetalsServerConfig
import scala.meta.internal.metals.ScalaVersions

import org.eclipse.lsp4j.jsonrpc.Launcher
import scala.meta.internal.metals.ThreadPools

object Main {
  def main(args: Array[String]): Unit = {
    if (args.exists(Set("-v", "--version", "-version"))) {
      val supportedScala2Versions =
        BuildInfo.supportedScala2Versions
          .groupBy(ScalaVersions.scalaBinaryVersionFromFullVersion)
          .toSeq
          .sortBy(_._1)
          .map { case (_, versions) => versions.mkString(", ") }
          .mkString("\n#       ")

      val supportedScala3Versions =
        BuildInfo.supportedScala3Versions.sorted.mkString(", ")

      println(
        s"""|metals ${BuildInfo.metalsVersion}
            |
            |# Note:
            |#   Supported Scala versions:
            |#     Scala 3: $supportedScala3Versions
            |#     Scala 2:
            |#       $supportedScala2Versions
            |""".stripMargin
      )

      sys.exit(0)
    }
    setupJna()
    val systemIn = System.in
    val systemOut = System.out
    val tracePrinter = GlobalTrace.setup("LSP")
    val exec = Executors.newCachedThreadPool()
    val sh = Executors.newSingleThreadScheduledExecutor()
    ThreadPools.discardRejectedRunnables("MetalsLanguageServer.ec", exec)
    ThreadPools.discardRejectedRunnables("MetalsLanguageServer.sh", sh)
    val ec = ExecutionContext.fromExecutorService(exec)
    val initialConfig = MetalsServerConfig.default
    val server = new MetalsLanguageServer(
      ec,
      redirectSystemOut = true,
      charset = StandardCharsets.UTF_8,
      initialConfig = initialConfig,
      sh = sh
    )
    try {
      scribe.info(s"Starting Metals server with configuration: $initialConfig")
      val launcher = new Launcher.Builder[MetalsLanguageClient]()
        .traceMessages(tracePrinter)
        .setExecutorService(exec)
        .setInput(systemIn)
        .setOutput(systemOut)
        .setRemoteInterface(classOf[MetalsLanguageClient])
        .setLocalService(server)
        .create()
      val clientProxy = launcher.getRemoteProxy
      server.connectToLanguageClient(clientProxy)
      launcher.startListening().get()
    } catch {
      case NonFatal(e) =>
        e.printStackTrace(systemOut)
        sys.exit(1)
    } finally {
      server.cancelAll()
    }
  }

  private def setupJna(): Unit = {
    // This is required to avoid the following error:
    //   java.lang.NoClassDefFoundError: Could not initialize class com.sun.jna.platform.win32.Kernel32
    //     at sbt.internal.io.WinMilli$.getHandle(Milli.scala:277)
    //   There is an incompatible JNA native library installed on this system
    //     Expected: 5.2.2
    //     Found:    3.2.1
    System.setProperty("jna.nosys", "true")
  }

}
