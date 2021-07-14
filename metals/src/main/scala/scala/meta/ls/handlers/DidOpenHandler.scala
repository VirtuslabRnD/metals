package scala.meta.ls.handlers

import org.eclipse.lsp4j.DidOpenTextDocumentParams
import java.util.concurrent.CompletableFuture
import scala.meta.io.AbsolutePath
import scala.meta.internal.metals.ActiveFiles
import scala.meta.internal.io.FileIO
import org.eclipse.lsp4j.ApplyWorkspaceEditParams
import scala.concurrent.Future
import scala.meta.internal.metals.CancelTokens
import scala.meta.internal.metals.MutableMd5Fingerprints
import scala.meta.internal.metals.PackageProvider
import scala.meta.internal.metals.Buffers
import scala.meta.internal.metals.DelegatingLanguageClient
import scala.meta.internal.metals.InteractiveSemanticdbs
import scala.meta.internal.metals.ammonite.Ammonite
import scala.meta.internal.metals.Compilers
import scala.meta.internal.metals.Compilations
import scala.meta.internal.metals.BatchedFunction
import java.nio.charset.Charset
import scala.concurrent.ExecutionContextExecutorService
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.decorations.SyntheticsDecorationProvider

final case class DidOpenHandler(
    focusedDocument: () => Option[AbsolutePath],
    setFocusedDocument: Option[AbsolutePath] => Unit,
    openedFiles: ActiveFiles,
    recentlyFocusedFiles: ActiveFiles,
    fingerprints: MutableMd5Fingerprints,
    packageProvider: PackageProvider,
    buffers: Buffers,
    languageClient: DelegatingLanguageClient,
    interactiveSemanticdbs: InteractiveSemanticdbs,
    ammonite: Ammonite,
    compilers: Compilers,
    compilations: Compilations,
    parseTrees: BatchedFunction[AbsolutePath, Unit],
    charset: Charset,
    executionContext: ExecutionContextExecutorService,
    syntheticsDecorator: SyntheticsDecorationProvider,
    workspace: () => AbsolutePath
) {

  private implicit def ec = executionContext

  def apply(params: DidOpenTextDocumentParams): CompletableFuture[Unit] = {
    val path = params.getTextDocument.getUri.toAbsolutePath
    // In some cases like peeking definition didOpen might be followed up by close
    // and we would lose the notion of the focused document
    focusedDocument().foreach(recentlyFocusedFiles.add)
    setFocusedDocument(Some(path))
    openedFiles.add(path)

    // Update md5 fingerprint from file contents on disk
    fingerprints.add(path, FileIO.slurp(path, charset))
    // Update in-memory buffer contents from LSP client
    buffers.put(path, params.getTextDocument.getText)

    packageProvider
      .workspaceEdit(path)
      .map(new ApplyWorkspaceEditParams(_))
      .foreach(languageClient.applyEdit)

    // trigger compilation in preparation for definition requests for dependency sources and standalone files
    val loadInteractive = Future {
      interactiveSemanticdbs.textDocument(path)
    }
    if (path.isDependencySource(workspace())) {
      CancelTokens.future { _ =>
        Future {
          // publish diagnostics
          interactiveSemanticdbs.didFocus(path)
          ()
        }(ec)
      }(ec)
    } else {
      if (path.isAmmoniteScript)
        ammonite.maybeImport(path)
      val loadFuture = compilers.load(List(path))
      val compileFuture =
        compilations.compileFile(path)
      Future
        .sequence(
          List(
            loadInteractive,
            parseTrees(path).flatMap(_ =>
              syntheticsDecorator.publishSynthetics(path)
            ),
            loadFuture,
            compileFuture
          )
        )
        .ignoreValue
        .asJava
    }
  }
}
