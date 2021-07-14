package scala.meta.ls.handlers

import java.util.concurrent.CompletableFuture
import org.eclipse.lsp4j.Hover
import scala.meta.internal.metals.CancelTokens
import org.eclipse.lsp4j.TextDocumentPositionParams
import scala.meta.internal.metals.Compilers
import scala.meta.internal.decorations.SyntheticsDecorationProvider
import scala.concurrent.ExecutionContextExecutorService
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.worksheets.WorksheetProvider

final case class TextDocumentHoverHandler(
    compilers: Compilers,
    syntheticsDecorator: SyntheticsDecorationProvider,
    executionContext: ExecutionContextExecutorService,
    worksheetProvider: WorksheetProvider
) {

  private implicit def ec = executionContext

  def apply(params: TextDocumentPositionParams): CompletableFuture[Hover] =
    CancelTokens.future { token =>
      compilers
        .hover(params, token)
        .map { hover =>
          syntheticsDecorator.addSyntheticsHover(params, hover)
        }
        .map(
          _.orElse {
            val path = params.getTextDocument.getUri.toAbsolutePath
            if (path.isWorksheet)
              worksheetProvider.hover(path, params.getPosition())
            else
              None
          }.orNull
        )
    }(ec)

}
