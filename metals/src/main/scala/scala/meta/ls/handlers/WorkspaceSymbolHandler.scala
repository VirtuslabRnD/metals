package scala.meta.ls.handlers

import org.eclipse.lsp4j.WorkspaceSymbolParams
import java.util
import java.util.concurrent.CompletableFuture
import scala.meta.internal.metals.CancelTokens
import scala.meta.internal.metals.Timer
import org.eclipse.lsp4j.SymbolInformation
import scala.concurrent.Promise
import scala.meta.internal.metals.Time
import scala.meta.internal.metals.WorkspaceSymbolProvider
import scala.meta.internal.metals.ClientConfiguration
import scala.meta.internal.metals.MetalsEnrichments._
import scala.concurrent.ExecutionContextExecutorService

final case class WorkspaceSymbolHandler(
    indexingPromise: Promise[Unit],
    time: Time,
    workspaceSymbols: WorkspaceSymbolProvider,
    clientConfig: ClientConfiguration,
    executionContext: ExecutionContextExecutorService
) {

  private implicit def ec = executionContext

  def apply(
      params: WorkspaceSymbolParams
  ): CompletableFuture[util.List[SymbolInformation]] =
    CancelTokens.future { token =>
      indexingPromise.future.map { _ =>
        val timer = new Timer(time)
        val result = workspaceSymbols.search(params.getQuery, token).asJava
        if (clientConfig.initialConfig.statistics.isWorkspaceSymbol) {
          scribe.info(
            s"time: found ${result.size()} results for query '${params.getQuery}' in $timer"
          )
        }
        result
      }
    }

}
