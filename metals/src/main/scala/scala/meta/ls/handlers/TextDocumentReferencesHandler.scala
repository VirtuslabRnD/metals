package scala.meta.ls.handlers

import java.util
import org.eclipse.lsp4j.ReferenceParams
import scala.meta.internal.metals.ReferencesResult
import scala.meta.internal.metals.Timer
import java.util.concurrent.CompletableFuture
import org.eclipse.lsp4j.Location
import scala.meta.internal.metals.Time
import scala.meta.internal.metals.CancelTokens
import scala.meta.internal.metals.ReferenceProvider
import scala.meta.internal.metals.ClientConfiguration
import scala.meta.internal.metals.Compilations
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.Buffers
import scala.meta.internal.parsing.TokenEditDistance
import scala.meta.internal.parsing.Trees
import scala.meta.internal.metals.StatusBar
import scala.concurrent.ExecutionContextExecutorService
import scala.concurrent.Future

final case class TextDocumentReferencesHandler(
    time: Time,
    referencesProvider: ReferenceProvider,
    clientConfig: ClientConfiguration,
    compilations: Compilations,
    buffers: Buffers,
    trees: Trees,
    statusBar: StatusBar,
    executionContext: ExecutionContextExecutorService
) {

  private implicit def ec = executionContext

  def apply(params: ReferenceParams): CompletableFuture[util.List[Location]] =
    CancelTokens.future { _ =>
      Future(referencesResult(params).locations.asJava)(ec)
    }(ec)

  def referencesResult(params: ReferenceParams): ReferencesResult = {
    val timer = new Timer(time)
    val result = referencesProvider.references(params)
    if (clientConfig.initialConfig.statistics.isReferences) {
      if (result.symbol.isEmpty)
        scribe.info(s"time: found 0 references in $timer")
      else
        scribe.info(
          s"time: found ${result.locations.length} references to symbol '${result.symbol}' in $timer"
        )
    }
    if (result.symbol.nonEmpty)
      compileAndLookForNewReferences(params, result)
    result
  }

  // Triggers a cascade compilation and tries to find new references to a given symbol.
  // It's not possible to stream reference results so if we find new symbols we notify the
  // user to run references again to see updated results.
  private def compileAndLookForNewReferences(
      params: ReferenceParams,
      result: ReferencesResult
  ): Unit = {
    val path = params.getTextDocument.getUri.toAbsolutePath
    val old = path.toInputFromBuffers(buffers)
    compilations.cascadeCompileFiles(Seq(path)).foreach { _ =>
      val newBuffer = path.toInputFromBuffers(buffers)
      val newParams: Option[ReferenceParams] =
        if (newBuffer.text == old.text) Some(params)
        else {
          val edit = TokenEditDistance(old, newBuffer, trees)
          edit
            .toRevised(
              params.getPosition.getLine,
              params.getPosition.getCharacter
            )
            .foldResult(
              pos => {
                params.getPosition.setLine(pos.startLine)
                params.getPosition.setCharacter(pos.startColumn)
                Some(params)
              },
              () => Some(params),
              () => None
            )
        }
      newParams match {
        case None =>
        case Some(p) =>
          val newResult = referencesProvider.references(p)
          val diff = newResult.locations.length - result.locations.length
          val isSameSymbol = newResult.symbol == result.symbol
          if (isSameSymbol && diff > 0) {
            import scala.meta.internal.semanticdb.Scala._
            val name = newResult.symbol.desc.name.value
            val message =
              s"Found new symbol references for '$name', try running again."
            scribe.info(message)
            statusBar
              .addMessage(clientConfig.icons.info + message)
          }
      }
    }
  }
}
