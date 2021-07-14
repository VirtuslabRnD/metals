package scala.meta.ls.handlers

import org.eclipse.lsp4j.TextDocumentPositionParams
import java.util.concurrent.CompletableFuture
import org.eclipse.lsp4j.Location
import scala.meta.internal.metals.CancelTokens
import scala.meta.pc.CancelToken
import scala.meta.internal.metals.EmptyCancelToken
import scala.concurrent.Future
import scala.concurrent.ExecutionContextExecutorService
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.DefinitionResult
import scala.meta.internal.mtags.Semanticdbs
import scala.meta.internal.metals.DefinitionProvider
import org.eclipse.lsp4j.ReferenceParams
import org.eclipse.lsp4j.ReferenceContext
import scala.meta.internal.metals.Warnings
import scala.meta.internal.metals.TimerProvider
import scala.meta.internal.metals.ClientConfiguration
import scala.meta.internal.metals.InteractiveSemanticdbs
import scala.util.Success

final case class TextDocumentDefinitionHandler(
    semanticdbs: Semanticdbs,
    executionContext: ExecutionContextExecutorService,
    definitionProvider: DefinitionProvider,
    textDocumentReferencesHandler: TextDocumentReferencesHandler,
    warnings: Warnings,
    timerProvider: TimerProvider,
    clientConfig: ClientConfiguration,
    interactiveSemanticdbs: InteractiveSemanticdbs
) {

  private implicit def ec = executionContext

  def apply(
      position: TextDocumentPositionParams
  ): CompletableFuture[java.util.List[Location]] =
    CancelTokens.future { token =>
      definitionOrReferences(position, token).map(_.locations)
    }(ec)

  /**
   * Returns the the definition location or reference locations of a symbol
   * at a given text document position.
   * If the symbol represents the definition itself, this method returns
   * the reference locations, otherwise this returns definition location.
   * https://github.com/scalameta/metals/issues/755
   */
  def definitionOrReferences(
      positionParams: TextDocumentPositionParams,
      token: CancelToken = EmptyCancelToken,
      definitionOnly: Boolean = false
  ): Future[DefinitionResult] = {
    val source = positionParams.getTextDocument.getUri.toAbsolutePath
    if (source.isScalaFilename) {
      val semanticDBDoc =
        semanticdbs.textDocument(source).documentIncludingStale
      (for {
        doc <- semanticDBDoc
        positionOccurrence = definitionProvider.positionOccurrence(
          source,
          positionParams.getPosition,
          doc
        )
        occ <- positionOccurrence.occurrence
      } yield occ) match {
        case Some(occ) =>
          if (occ.role.isDefinition && !definitionOnly) {
            val refParams = new ReferenceParams(
              positionParams.getTextDocument(),
              positionParams.getPosition(),
              new ReferenceContext(false)
            )
            val result =
              textDocumentReferencesHandler.referencesResult(refParams)
            if (result.locations.isEmpty) {
              // Fallback again to the original behavior that returns
              // the definition location itself if no reference locations found,
              // for avoiding the confusing messages like "No definition found ..."
              definitionResult(positionParams, token)
            } else {
              Future.successful(
                DefinitionResult(
                  locations = result.locations.asJava,
                  symbol = result.symbol,
                  definition = None,
                  semanticdb = None
                )
              )
            }
          } else {
            definitionResult(positionParams, token)
          }
        case None =>
          if (semanticDBDoc.isEmpty) {
            warnings.noSemanticdb(source)
          }
          // Even if it failed to retrieve the symbol occurrence from semanticdb,
          // try to find its definitions from presentation compiler.
          definitionResult(positionParams, token)
      }
    } else {
      // Ignore non-scala files.
      Future.successful(DefinitionResult.empty)
    }
  }

  /**
   * Returns textDocument/definition in addition to the resolved symbol.
   *
   * The resolved symbol is used for testing purposes only.
   */
  def definitionResult(
      position: TextDocumentPositionParams,
      token: CancelToken = EmptyCancelToken
  ): Future[DefinitionResult] = {
    val source = position.getTextDocument.getUri.toAbsolutePath
    if (source.isScalaFilename) {
      val result =
        timerProvider.timedThunk(
          "definition",
          clientConfig.initialConfig.statistics.isDefinition
        )(
          definitionProvider.definition(source, position, token)
        )
      result.onComplete {
        case Success(value) =>
          // Record what build target this dependency source (if any) was jumped from,
          // needed to know what classpath to compile the dependency source with.
          interactiveSemanticdbs.didDefinition(source, value)
        case _ =>
      }
      result
    } else {
      // Ignore non-scala files.
      Future.successful(DefinitionResult.empty)
    }
  }

}
