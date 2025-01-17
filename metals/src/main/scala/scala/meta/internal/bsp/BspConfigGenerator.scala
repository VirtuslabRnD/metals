package scala.meta.internal.bsp

import scala.concurrent.Future

import scala.meta.internal.bsp.BspConfigGenerationStatus._
import scala.meta.internal.builds.BuildServerProvider
import scala.meta.internal.builds.BuildTool
import scala.meta.internal.builds.BuildTools
import scala.meta.internal.builds.ShellRunner
import scala.meta.internal.metals.Messages.BspProvider
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.MetalsLanguageClient
import scala.meta.io.AbsolutePath

import org.eclipse.lsp4j.MessageActionItem
import scala.meta.ls.MetalsThreads

/**
 * Runs a process to create a .bsp entry for a givev buildtool.
 */
final case class BspConfigGenerator(
    workspace: () => AbsolutePath,
    languageClient: MetalsLanguageClient,
    buildTools: BuildTools,
    shellRunner: ShellRunner,
    threads: MetalsThreads
) {
  def runUnconditionally(
      buildTool: BuildTool,
      args: List[String]
  ): Future[BspConfigGenerationStatus] = {
    shellRunner
      .run(
        s"${buildTool.executableName} bspConfig",
        args,
        workspace(),
        buildTool.redirectErrorOutput
      )
      .map(BspConfigGenerationStatus.fromExitCode)(threads.dummyEc)
  }

  /**
   * Given multiple build tools that are all BuildServerProviders, allow the
   * choose the desired build server and then connect to it.
   */
  def chooseAndGenerate(
      buildTools: List[BuildTool with BuildServerProvider]
  ): Future[(BuildTool, BspConfigGenerationStatus)] = {
    implicit val ec0 = threads.dummyEc
    for {
      Some(buildTool) <- chooseBuildServerProvider(buildTools)
      status <- buildTool.generateBspConfig(
        workspace(),
        languageClient,
        args => runUnconditionally(buildTool, args)
      )
    } yield (buildTool, status)
  }

  private def chooseBuildServerProvider(
      buildTools: List[BuildTool with BuildServerProvider]
  ): Future[Option[BuildTool with BuildServerProvider]] = {
    languageClient
      .showMessageRequest(BspProvider.params(buildTools))
      .asScala
      .map { choice =>
        buildTools.find(buildTool =>
          new MessageActionItem(buildTool.executableName) == choice
        )
      }(threads.dummyEc)
  }
}
