package scala.meta.internal.metals

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import scala.meta.internal.metals.codeactions._
import scala.meta.internal.parsing.Trees
import scala.meta.pc.CancelToken

import org.eclipse.{lsp4j => l}

final case class CodeActionProvider(
    compilers: Compilers,
    buffers: Buffers,
    buildTargets: BuildTargets,
    scalafixProvider: ScalafixProvider,
    trees: Trees,
    diagnostics: Diagnostics,
    languageClient: MetalsLanguageClient
)(ec: ExecutionContext) {

  private val extractMemberAction = new ExtractRenameMember(buffers, trees)(ec)
  private val createNewSymbolActions = new CreateNewSymbol()
  private val stringActions = new StringActions(buffers, trees)

  private def allActions(): List[CodeAction] = List(
    new ImplementAbstractMembers(compilers),
    new ImportMissingSymbol(compilers),
    createNewSymbolActions,
    stringActions,
    extractMemberAction,
    new SourceOrganizeImports(
      scalafixProvider,
      buildTargets,
      diagnostics,
      languageClient
    )(ec),
    new OrganizeImportsQuickFix(
      scalafixProvider,
      buildTargets,
      diagnostics
    )(ec),
    new InsertInferredType(trees, compilers)
  )

  def codeActions(
      params: l.CodeActionParams,
      token: CancelToken
  )(implicit ec: ExecutionContext): Future[Seq[l.CodeAction]] = {
    def isRequestedKind(action: CodeAction): Boolean =
      Option(params.getContext.getOnly) match {
        case Some(only) =>
          only.asScala.toSet.exists(requestedKind =>
            action.kind.startsWith(requestedKind)
          )
        case None => true
      }

    val actions = allActions().collect {
      case action if isRequestedKind(action) => action.contribute(params, token)
    }

    Future.sequence(actions).map(_.flatten)
  }

  def executeCommands(
      codeActionCommandData: CodeActionCommandData,
      token: CancelToken
  ): Future[CodeActionCommandResult] = {
    codeActionCommandData match {
      case data: ExtractMemberDefinitionData =>
        extractMemberAction.executeCommand(data)
      case data => Future.failed(new IllegalArgumentException(data.toString))
    }
  }
}
