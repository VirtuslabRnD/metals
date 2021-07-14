package scala.meta.ls.handlers

import java.util.concurrent.CompletableFuture
import ch.epfl.scala.{bsp4j => b}
import scala.meta.internal.metals.DidFocusResult
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.io.AbsolutePath
import scala.meta.internal.metals.BuildTargets
import java.util.concurrent.atomic.AtomicReference
import scala.meta.internal.metals.InteractiveSemanticdbs
import scala.meta.internal.metals.ActiveFiles
import scala.meta.internal.decorations.SyntheticsDecorationProvider
import scala.meta.internal.worksheets.WorksheetProvider
import scala.meta.internal.metals.Compilations
import scala.concurrent.ExecutionContextExecutorService

final case class MetalsDidFocusTextDocumentHandler(
    setFocusedDocument: AbsolutePath => Unit,
    buildTargets: BuildTargets,
    focusedDocumentBuildTarget: AtomicReference[b.BuildTargetIdentifier],
    interactiveSemanticdbs: InteractiveSemanticdbs,
    workspace: () => AbsolutePath,
    openedFiles: ActiveFiles,
    syntheticsDecorator: SyntheticsDecorationProvider,
    worksheetProvider: WorksheetProvider,
    compilations: Compilations,
    executionContext: ExecutionContextExecutorService
) {

  private implicit def ec = executionContext

  def apply(params: AnyRef): CompletableFuture[DidFocusResult.Value] = {

    val uriOpt: Option[String] = params match {
      case string: String =>
        Option(string)
      case (h: String) :: Nil =>
        Option(h)
      case _ =>
        scribe.warn(
          s"Unexpected notification params received for didFocusTextDocument: $params"
        )
        None
    }

    uriOpt match {
      case Some(uri) => {
        val path = uri.toAbsolutePath
        setFocusedDocument(path)
        buildTargets
          .inverseSources(path)
          .foreach(focusedDocumentBuildTarget.set)

        // unpublish diagnostic for dependencies
        interactiveSemanticdbs.didFocus(path)
        // Don't trigger compilation on didFocus events under cascade compilation
        // because save events already trigger compile in inverse dependencies.
        if (path.isDependencySource(workspace())) {
          CompletableFuture.completedFuture(DidFocusResult.NoBuildTarget)
        } else if (openedFiles.isRecentlyActive(path)) {
          CompletableFuture.completedFuture(DidFocusResult.RecentlyActive)
        } else {
          syntheticsDecorator.publishSynthetics(path)
          worksheetProvider.onDidFocus(path)
          buildTargets.inverseSources(path) match {
            case Some(target) =>
              val isAffectedByCurrentCompilation =
                path.isWorksheet ||
                  buildTargets.isInverseDependency(
                    target,
                    compilations.currentlyCompiling.toList
                  )

              def isAffectedByLastCompilation: Boolean =
                !compilations.wasPreviouslyCompiled(target) &&
                  buildTargets.isInverseDependency(
                    target,
                    compilations.previouslyCompiled.toList
                  )

              val needsCompile =
                isAffectedByCurrentCompilation || isAffectedByLastCompilation
              if (needsCompile) {
                compilations
                  .compileFile(path)
                  .map(_ => DidFocusResult.Compiled)
                  .asJava
              } else {
                CompletableFuture.completedFuture(
                  DidFocusResult.AlreadyCompiled
                )
              }
            case None =>
              CompletableFuture.completedFuture(DidFocusResult.NoBuildTarget)
          }
        }
      }
      case None =>
        CompletableFuture.completedFuture(DidFocusResult.NoBuildTarget)
    }
  }
}
