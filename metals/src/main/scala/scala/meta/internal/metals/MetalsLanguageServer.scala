package scala.meta.internal.metals

import java.net.URI
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets
import java.nio.file._
import java.util
import java.util.concurrent.CompletableFuture
import java.util.concurrent.Executors
import java.util.concurrent.ScheduledExecutorService
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicReference

import scala.concurrent.Await
import scala.concurrent.ExecutionContextExecutorService
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.concurrent.TimeoutException
import scala.concurrent.duration.Duration
import scala.concurrent.duration._
import scala.util.Success
import scala.util.control.NonFatal

import scala.meta.internal.bsp.BspConfigGenerator
import scala.meta.internal.bsp.BspConnector
import scala.meta.internal.bsp.BspServers
import scala.meta.internal.bsp.BspSession
import scala.meta.internal.bsp.BuildChange
import scala.meta.internal.builds.BloopInstall
import scala.meta.internal.builds.BuildToolSelector
import scala.meta.internal.builds.BuildTools
import scala.meta.internal.builds.NewProjectProvider
import scala.meta.internal.builds.ShellRunner
import scala.meta.internal.builds.WorkspaceReload
import scala.meta.internal.decorations.SyntheticsDecorationProvider
import scala.meta.internal.implementation.ImplementationProvider
import scala.meta.internal.implementation.Supermethods
import scala.meta.internal.io.FileIO
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.ammonite.Ammonite
import scala.meta.internal.metals.codelenses.RunTestCodeLens
import scala.meta.internal.metals.codelenses.SuperMethodCodeLens
import scala.meta.internal.metals.codelenses.WorksheetCodeLens
import scala.meta.internal.metals.debug.BuildTargetClasses
import scala.meta.internal.metals.debug.DebugProvider
import scala.meta.internal.metals.newScalaFile.NewFileProvider
import scala.meta.internal.mtags._
import scala.meta.internal.parsing.ClassFinder
import scala.meta.internal.parsing.DocumentSymbolProvider
import scala.meta.internal.parsing.FoldingRangeProvider
import scala.meta.internal.parsing.Trees
import scala.meta.internal.remotels.RemoteLanguageServer
import scala.meta.internal.rename.RenameProvider
import scala.meta.internal.tvp._
import scala.meta.internal.watcher.DirectoryChangeEvent
import scala.meta.internal.watcher.DirectoryChangeEvent.EventType
import scala.meta.internal.worksheets.DecorationWorksheetPublisher
import scala.meta.internal.worksheets.WorksheetProvider
import scala.meta.internal.worksheets.WorkspaceEditWorksheetPublisher
import scala.meta.io.AbsolutePath
import scala.meta.parsers.ParseException
import scala.meta.tokenizers.TokenizeException

import ch.epfl.scala.bsp4j.CompileReport
import ch.epfl.scala.{bsp4j => b}
import io.undertow.server.HttpServerExchange
import org.eclipse.lsp4j.ExecuteCommandParams
import org.eclipse.lsp4j._
import org.eclipse.lsp4j.jsonrpc.messages.{Either => JEither}
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification
import org.eclipse.lsp4j.jsonrpc.services.JsonRequest
import org.eclipse.{lsp4j => l}
import scala.meta.ls.handlers.DidOpenHandler
import scala.meta.ls.handlers.MetalsDidFocusTextDocumentHandler
import scala.meta.ls.handlers.WorkspaceDidChangeConfigurationHandler
import scala.meta.ls.handlers.TextDocumentReferencesHandler
import scala.meta.ls.handlers.TextDocumentDefinitionHandler
import scala.meta.ls.handlers.TextDocumentHoverHandler
import scala.meta.ls.handlers.WorkspaceSymbolHandler
import scala.meta.ls.handlers.ExecuteCommandHandler

class MetalsLanguageServer(
    ec: ExecutionContextExecutorService,
    buffers: Buffers = Buffers(),
    redirectSystemOut: Boolean = true,
    charset: Charset = StandardCharsets.UTF_8,
    time: Time = Time.system,
    initialConfig: MetalsServerConfig = MetalsServerConfig.default,
    progressTicks: ProgressTicks = ProgressTicks.braille,
    bspGlobalDirectories: List[AbsolutePath] =
      BspServers.globalInstallDirectories,
    sh: ScheduledExecutorService = Executors.newSingleThreadScheduledExecutor(),
    isReliableFileWatcher: Boolean = true
) extends Cancelable {
  private val cancelables = new MutableCancelable()
  val isCancelled = new AtomicBoolean(false)
  override def cancel(): Unit = {
    if (isCancelled.compareAndSet(false, true)) {
      val buildShutdown = bspSession match {
        case Some(session) => session.shutdown()
        case None => Future.successful(())
      }
      try cancelables.cancel()
      catch {
        case NonFatal(_) =>
      }
      try buildShutdown.asJava.get(100, TimeUnit.MILLISECONDS)
      catch {
        case _: TimeoutException =>
      }
    }
  }

  def cancelAll(): Unit = {
    cancel()
    Cancelable.cancelAll(
      List(
        Cancelable(() => ec.shutdown()),
        Cancelable(() => sh.shutdown())
      )
    )
  }

  private var workspaceOpt = Option.empty[AbsolutePath]

  private def workspace: AbsolutePath =
    workspaceOpt.getOrElse {
      sys.error(
        "MetalsLanguageServer is not initialized yet, workspace not available"
      )
    }

  private implicit val executionContext: ExecutionContextExecutorService = ec

  private val fingerprints = new MutableMd5Fingerprints
  private val mtags = new Mtags
  var focusedDocument: Option[AbsolutePath] = None
  private val focusedDocumentBuildTarget =
    new AtomicReference[b.BuildTargetIdentifier]()
  private val definitionIndex = newSymbolIndex()
  private val symbolDocs = new Docstrings(definitionIndex)
  var bspSession: Option[BspSession] =
    Option.empty[BspSession]
  private val savedFiles = new ActiveFiles(time)
  private val openedFiles = new ActiveFiles(time)
  private val recentlyFocusedFiles = new ActiveFiles(time)
  private val languageClient = new DelegatingLanguageClient(NoopLanguageClient)
  var userConfig: UserConfiguration = UserConfiguration()
  val excludedPackageHandler: ExcludedPackagesHandler =
    new ExcludedPackagesHandler(userConfig.excludedPackages)
  var ammonite: Ammonite = _
  private val mainBuildTargetsData = BuildTargets.Data.create()
  val buildTargets: BuildTargets =
    BuildTargets.withAmmonite(() => workspace, Some(tables), () => ammonite)
  buildTargets.addData(mainBuildTargetsData)
  private val buildTargetClasses =
    new BuildTargetClasses(buildTargets)

  private val scalaVersionSelector = new ScalaVersionSelector(
    () => userConfig,
    buildTargets
  )
  private val remote = RemoteLanguageServer(
    () => workspace,
    () => userConfig,
    initialConfig,
    buffers,
    buildTargets
  )
  val compilations: Compilations = new Compilations(
    buildTargets,
    buildTargetClasses,
    () => workspace,
    languageClient,
    buildTarget => focusedDocumentBuildTarget.get() == buildTarget,
    worksheets => onWorksheetChanged(worksheets)
  )
  private val fileWatcher = register(
    new FileWatcher(
      buildTargets,
      params => didChangeWatchedFiles(params)
    )
  )
  private val indexingPromise: Promise[Unit] = Promise[Unit]()
  private val buildServerPromise: Promise[Unit] = Promise[Unit]()
  val parseTrees = new BatchedFunction[AbsolutePath, Unit](paths =>
    CancelableFuture(
      buildServerPromise.future
        .flatMap(_ => parseTreesAndPublishDiags(paths))
        .ignoreValue,
      Cancelable.empty
    )
  )
  private val onBuildChanged =
    BatchedFunction.fromFuture[AbsolutePath, BuildChange](
      onBuildChangedUnbatched
    )
  val pauseables: Pauseable = Pauseable.fromPausables(
    onBuildChanged ::
      parseTrees ::
      compilations.pauseables
  )
  private val timerProvider = TimerProvider(time)
  private val trees = new Trees(buildTargets, buffers, scalaVersionSelector)
  private val documentSymbolProvider = new DocumentSymbolProvider(trees)
  private val multilineStringFormattingProvider =
    new MultilineStringFormattingProvider(buffers, trees, () => userConfig)
  private val classFinder = new ClassFinder(trees)
  private val foldingRangeProvider = new FoldingRangeProvider(trees, buffers)
  // These can't be instantiated until we know the workspace root directory.
  private var shellRunner: ShellRunner = _
  private var bloopInstall: BloopInstall = _
  private var bspConfigGenerator: BspConfigGenerator = _
  private var diagnostics: Diagnostics = _
  private var warnings: Warnings = _
  private var fileSystemSemanticdbs: FileSystemSemanticdbs = _
  private var interactiveSemanticdbs: InteractiveSemanticdbs = _
  private var buildTools: BuildTools = _
  private var newProjectProvider: NewProjectProvider = _
  private var semanticdbs: Semanticdbs = _
  private var buildClient: ForwardingMetalsBuildClient = _
  private var bloopServers: BloopServers = _
  private var bspServers: BspServers = _
  private var bspConnector: BspConnector = _
  private var codeLensProvider: CodeLensProvider = _
  private var supermethods: Supermethods = _
  private var codeActionProvider: CodeActionProvider = _
  private var definitionProvider: DefinitionProvider = _
  private var semanticDBIndexer: SemanticdbIndexer = _
  private var implementationProvider: ImplementationProvider = _
  private var renameProvider: RenameProvider = _
  private var documentHighlightProvider: DocumentHighlightProvider = _
  private var formattingProvider: FormattingProvider = _
  private var syntheticsDecorator: SyntheticsDecorationProvider = _
  private var initializeParams: Option[InitializeParams] = None
  private var referencesProvider: ReferenceProvider = _
  private var workspaceSymbols: WorkspaceSymbolProvider = _
  private val packageProvider: PackageProvider =
    new PackageProvider(buildTargets)
  private var newFileProvider: NewFileProvider = _
  private var debugProvider: DebugProvider = _
  private var symbolSearch: MetalsSymbolSearch = _
  private var compilers: Compilers = _
  private var scalafixProvider: ScalafixProvider = _
  private var workspaceReload: WorkspaceReload = _
  private var buildToolSelector: BuildToolSelector = _

  private val sourceMapper = scala.meta.ls.SourceMapper(
    buildTargets,
    buffers,
    () => workspace
  )

  def loadedPresentationCompilerCount(): Int =
    compilers.loadedPresentationCompilerCount()
  var tables: Tables = _
  var statusBar: StatusBar = _
  private var embedded: Embedded = _
  private var doctor: Doctor = _
  var httpServer: Option[MetalsHttpServer] = None
  var treeView: TreeViewProvider = NoopTreeViewProvider
  var worksheetProvider: WorksheetProvider = _
  var popupChoiceReset: PopupChoiceReset = _
  var stacktraceAnalyzer: StacktraceAnalyzer = _

  private val clientConfig: ClientConfiguration =
    new ClientConfiguration(
      initialConfig,
      ClientExperimentalCapabilities.Default,
      InitializationOptions.Default
    )

  def parseTreesAndPublishDiags(paths: Seq[AbsolutePath]): Future[Seq[Unit]] = {
    Future.traverse(paths.distinct) { path =>
      if (path.isScalaFilename) {
        Future(diagnostics.onSyntaxError(path, trees.didChange(path)))
      } else {
        Future.successful(())
      }
    }
  }

  def connectToLanguageClient(client: MetalsLanguageClient): Unit = {
    languageClient.underlying =
      new ConfiguredLanguageClient(client, clientConfig)(ec)
    statusBar = new StatusBar(
      languageClient,
      time,
      progressTicks,
      clientConfig
    )
    embedded = register(
      new Embedded(
        statusBar,
        () => userConfig
      )
    )
    LanguageClientLogger.languageClient = Some(languageClient)
    cancelables.add(() => languageClient.shutdown())
  }

  def register[T <: Cancelable](cancelable: T): T = {
    cancelables.add(cancelable)
    cancelable
  }

  private def updateWorkspaceDirectory(params: InitializeParams): Unit = {

    // NOTE: we purposefully don't check workspaceFolders here
    // since Metals technically doesn't support it. Once we implement
    // https://github.com/scalameta/metals-feature-requests/issues/87 we'll
    // have to change this.
    val root =
      Option(params.getRootUri()).orElse(Option(params.getRootPath()))

    root match {
      case None =>
        languageClient.showMessage(Messages.noRoot)
      case Some(path) =>
        workspaceOpt = Some(AbsolutePath(Paths.get(URI.create(path))).dealias)
        MetalsLogger.setupLspLogger(workspace, redirectSystemOut)

        val clientInfo = Option(params.getClientInfo()) match {
          case Some(info) =>
            s"for client ${info.getName()} ${Option(info.getVersion).getOrElse("")}"
          case None => ""
        }

        scribe.info(
          s"Started: Metals version ${BuildInfo.metalsVersion} in workspace '$workspace' $clientInfo."
        )

        clientConfig.experimentalCapabilities =
          ClientExperimentalCapabilities.from(params.getCapabilities)
        clientConfig.initializationOptions = InitializationOptions.from(params)

        foldingRangeProvider.setFoldOnlyLines(Option(params).foldOnlyLines)
        documentSymbolProvider.setSupportsHierarchicalDocumentSymbols(
          initializeParams.supportsHierarchicalDocumentSymbols
        )
        tables = register(new Tables(() => workspace, time, clientConfig))
        workspaceReload = new WorkspaceReload(
          () => workspace,
          languageClient,
          tables
        )
        buildTools = new BuildTools(
          () => workspace,
          bspGlobalDirectories,
          () => userConfig,
          () => tables.buildServers.selectedServer().nonEmpty
        )
        fileSystemSemanticdbs = new FileSystemSemanticdbs(
          buildTargets,
          charset,
          () => workspace,
          fingerprints
        )
        interactiveSemanticdbs = register(
          new InteractiveSemanticdbs(
            () => workspace,
            buildTargets,
            charset,
            languageClient,
            tables,
            statusBar,
            () => compilers,
            clientConfig,
            () => semanticDBIndexer
          )
        )
        warnings = new Warnings(
          () => workspace,
          buildTargets,
          statusBar,
          clientConfig.icons,
          buildTools,
          compilations.isCurrentlyCompiling
        )
        diagnostics = new Diagnostics(
          buffers,
          languageClient,
          clientConfig.initialConfig.statistics,
          () => userConfig,
          () => Option(workspace),
          trees
        )
        buildClient = new ForwardingMetalsBuildClient(
          languageClient,
          diagnostics,
          buildTargets,
          buildTargetClasses,
          clientConfig,
          statusBar,
          time,
          report => {
            didCompileTarget(report)
            compilers.didCompile(report)
          },
          () => treeView,
          () => worksheetProvider,
          () => ammonite
        )
        shellRunner = register(
          new ShellRunner(languageClient, () => userConfig, time, statusBar)
        )
        bloopInstall = new BloopInstall(
          () => workspace,
          languageClient,
          buildTools,
          tables,
          shellRunner
        )
        bspConfigGenerator = new BspConfigGenerator(
          () => workspace,
          languageClient,
          buildTools,
          shellRunner
        )
        newProjectProvider = new NewProjectProvider(
          languageClient,
          statusBar,
          clientConfig,
          shellRunner,
          clientConfig.icons,
          () => workspace
        )
        bloopServers = new BloopServers(
          buildClient,
          languageClient,
          tables,
          clientConfig.initialConfig
        )
        bspServers = new BspServers(
          () => workspace,
          charset,
          languageClient,
          buildClient,
          tables,
          bspGlobalDirectories,
          clientConfig.initialConfig
        )
        buildToolSelector = new BuildToolSelector(
          languageClient,
          tables
        )
        bspConnector = new BspConnector(
          bloopServers,
          bspServers,
          buildTools,
          languageClient,
          tables,
          () => userConfig,
          statusBar
        )
        semanticdbs = AggregateSemanticdbs(
          List(
            fileSystemSemanticdbs,
            interactiveSemanticdbs
          )
        )
        definitionProvider = new DefinitionProvider(
          () => workspace,
          mtags,
          buffers,
          definitionIndex,
          semanticdbs,
          warnings,
          () => compilers,
          remote,
          trees,
          buildTargets,
          scalaVersionSelector
        )
        formattingProvider = new FormattingProvider(
          () => workspace,
          buffers,
          () => userConfig,
          languageClient,
          clientConfig,
          statusBar,
          clientConfig.icons,
          tables,
          buildTargets
        )
        newFileProvider = new NewFileProvider(
          () => workspace,
          languageClient,
          packageProvider,
          () => focusedDocument
        )
        referencesProvider = new ReferenceProvider(
          () => workspace,
          semanticdbs,
          buffers,
          definitionProvider,
          remote,
          trees
        )
        implementationProvider = new ImplementationProvider(
          semanticdbs,
          () => workspace,
          definitionIndex,
          buildTargets,
          buffers,
          definitionProvider,
          trees,
          scalaVersionSelector
        )

        supermethods = new Supermethods(
          languageClient,
          definitionProvider,
          implementationProvider
        )

        val runTestLensProvider =
          new RunTestCodeLens(
            buildTargetClasses,
            buffers,
            buildTargets,
            clientConfig,
            () => bspSession.map(_.main.hasDebug).getOrElse(false),
            trees
          )

        val goSuperLensProvider = new SuperMethodCodeLens(
          implementationProvider,
          buffers,
          () => userConfig,
          clientConfig,
          trees
        )

        stacktraceAnalyzer = new StacktraceAnalyzer(
          () => workspace,
          buffers,
          definitionProvider,
          clientConfig.icons,
          clientConfig.isCommandInHtmlSupported
        )
        val worksheetCodeLens = new WorksheetCodeLens(clientConfig)
        codeLensProvider = new CodeLensProvider(
          List(runTestLensProvider, goSuperLensProvider, worksheetCodeLens),
          semanticdbs,
          stacktraceAnalyzer
        )
        renameProvider = new RenameProvider(
          referencesProvider,
          implementationProvider,
          definitionProvider,
          () => workspace,
          languageClient,
          buffers,
          compilations,
          clientConfig,
          trees
        )
        syntheticsDecorator = new SyntheticsDecorationProvider(
          () => workspace,
          semanticdbs,
          buffers,
          languageClient,
          fingerprints,
          charset,
          () => focusedDocument,
          clientConfig,
          () => userConfig,
          trees
        )
        semanticDBIndexer = new SemanticdbIndexer(
          referencesProvider,
          implementationProvider,
          syntheticsDecorator,
          buildTargets,
          () => workspace
        )
        documentHighlightProvider = new DocumentHighlightProvider(
          definitionProvider,
          semanticdbs
        )
        workspaceSymbols = new WorkspaceSymbolProvider(
          () => workspace,
          buildTargets,
          definitionIndex,
          excludedPackageHandler.isExcludedPackage
        )
        symbolSearch = new MetalsSymbolSearch(
          () => workspace,
          symbolDocs,
          workspaceSymbols,
          definitionProvider
        )
        compilers = register(
          new Compilers(
            () => workspace,
            clientConfig,
            () => userConfig,
            buildTargets,
            buffers,
            symbolSearch,
            embedded,
            statusBar,
            sh,
            Option(params),
            excludedPackageHandler.isExcludedPackage,
            scalaVersionSelector,
            trees,
            sourceMapper
          )
        )
        debugProvider = new DebugProvider(
          () => workspace,
          definitionProvider,
          () => bspSession.map(_.mainConnection),
          buildTargets,
          buildTargetClasses,
          compilations,
          languageClient,
          buildClient,
          classFinder,
          definitionIndex,
          stacktraceAnalyzer,
          clientConfig,
          semanticdbs
        )
        scalafixProvider = new ScalafixProvider(
          buffers,
          () => userConfig,
          () => workspace,
          statusBar,
          compilations,
          languageClient,
          buildTargets,
          buildClient
        )
        codeActionProvider = new CodeActionProvider(
          compilers,
          buffers,
          buildTargets,
          scalafixProvider,
          trees,
          diagnostics,
          languageClient
        )
        doctor = new Doctor(
          () => workspace,
          buildTargets,
          languageClient,
          () => bspSession,
          () => bspConnector.resolve(),
          () => httpServer,
          tables,
          clientConfig
        )
        popupChoiceReset = new PopupChoiceReset(
          () => workspace,
          tables,
          languageClient,
          doctor,
          () => buildServerManager.slowConnectToBuildServer(forceImport = true),
          bspConnector,
          () => buildServerManager.quickConnectToBuildServer()
        )

        val worksheetPublisher =
          if (clientConfig.isDecorationProvider)
            DecorationWorksheetPublisher
          else
            new WorkspaceEditWorksheetPublisher(buffers, trees)
        worksheetProvider = register(
          new WorksheetProvider(
            () => workspace,
            buffers,
            buildTargets,
            languageClient,
            () => userConfig,
            statusBar,
            diagnostics,
            embedded,
            worksheetPublisher,
            compilers,
            compilations,
            scalaVersionSelector
          )
        )
        ammonite = register(
          new Ammonite(
            buffers,
            compilers,
            compilations,
            statusBar,
            diagnostics,
            doctor,
            () => tables,
            languageClient,
            buildClient,
            () => userConfig,
            () => indexer.profiledIndexWorkspace(() => ()),
            () => workspace,
            () => focusedDocument,
            buildTargets,
            () => buildTools,
            clientConfig.initialConfig,
            scalaVersionSelector
          )
        )
        if (clientConfig.isTreeViewProvider) {
          treeView = new MetalsTreeViewProvider(
            () => workspace,
            languageClient,
            buildTargets,
            () => buildClient.ongoingCompilations(),
            definitionIndex,
            clientConfig.initialConfig.statistics,
            id => compilations.compileTarget(id),
            sh,
            () => bspSession.map(_.mainConnectionIsBloop).getOrElse(false)
          )
        }
    }
  }

  @JsonRequest("initialize")
  def initialize(
      params: InitializeParams
  ): CompletableFuture[InitializeResult] = {
    timerProvider
      .timed("initialize")(Future {
        initializeParams = Option(params)
        updateWorkspaceDirectory(params)
        val capabilities = new ServerCapabilities()
        capabilities.setExecuteCommandProvider(
          new ExecuteCommandOptions(
            ServerCommands.all.map(_.id).asJava
          )
        )
        capabilities.setFoldingRangeProvider(true)
        capabilities.setSelectionRangeProvider(true)
        capabilities.setCodeLensProvider(new CodeLensOptions(false))
        capabilities.setDefinitionProvider(true)
        capabilities.setImplementationProvider(true)
        capabilities.setHoverProvider(true)
        capabilities.setReferencesProvider(true)
        val renameOptions = new RenameOptions()
        renameOptions.setPrepareProvider(true)
        capabilities.setRenameProvider(renameOptions)
        capabilities.setDocumentHighlightProvider(true)
        capabilities.setDocumentOnTypeFormattingProvider(
          new DocumentOnTypeFormattingOptions("\n", List("\"").asJava)
        )
        capabilities.setDocumentRangeFormattingProvider(
          initialConfig.allowMultilineStringFormatting
        )
        capabilities.setSignatureHelpProvider(
          new SignatureHelpOptions(List("(", "[", ",").asJava)
        )
        capabilities.setCompletionProvider(
          new CompletionOptions(
            clientConfig.isCompletionItemResolve,
            List(".", "*").asJava
          )
        )
        capabilities.setWorkspaceSymbolProvider(true)
        capabilities.setDocumentSymbolProvider(true)
        capabilities.setDocumentFormattingProvider(true)
        if (initializeParams.supportsCodeActionLiterals) {
          capabilities.setCodeActionProvider(
            new CodeActionOptions(
              List(
                CodeActionKind.QuickFix,
                CodeActionKind.Refactor,
                CodeActionKind.SourceOrganizeImports
              ).asJava
            )
          )
        } else {
          capabilities.setCodeActionProvider(true)
        }

        val textDocumentSyncOptions = new TextDocumentSyncOptions
        textDocumentSyncOptions.setChange(TextDocumentSyncKind.Full)
        textDocumentSyncOptions.setSave(new SaveOptions(true))
        textDocumentSyncOptions.setOpenClose(true)

        capabilities.setTextDocumentSync(textDocumentSyncOptions)

        val serverInfo = new ServerInfo("Metals", BuildInfo.metalsVersion)
        new InitializeResult(capabilities, serverInfo)
      })
      .asJava
  }

  private def registerNiceToHaveFilePatterns(): Unit = {
    for {
      params <- initializeParams
      capabilities <- Option(params.getCapabilities)
      workspace <- Option(capabilities.getWorkspace)
      didChangeWatchedFiles <- Option(workspace.getDidChangeWatchedFiles)
      if didChangeWatchedFiles.getDynamicRegistration
    } yield {
      languageClient.registerCapability(
        new RegistrationParams(
          List(
            new Registration(
              "1",
              "workspace/didChangeWatchedFiles",
              clientConfig.globSyntax.registrationOptions(
                this.workspace
              )
            )
          ).asJava
        )
      )
    }
  }

  private def startHttpServer(): Unit = {
    if (clientConfig.isHttpEnabled) {
      val host = "localhost"
      val port = 5031
      var url = s"http://$host:$port"
      var render: () => String = () => ""
      var completeCommand: HttpServerExchange => Unit = (_) => ()
      val server = register(
        MetalsHttpServer(
          host,
          port,
          this,
          () => render(),
          e => completeCommand(e),
          () => doctor.problemsHtmlPage(url)
        )
      )
      httpServer = Some(server)
      val newClient = new MetalsHttpClient(
        workspace,
        () => url,
        languageClient.underlying,
        () => server.reload(),
        charset,
        clientConfig.icons,
        time,
        sh,
        clientConfig
      )
      render = () => newClient.renderHtml
      completeCommand = e => newClient.completeCommand(e)
      languageClient.underlying = newClient
      server.start()
      url = server.address
    }
  }

  val isInitialized = new AtomicBoolean(false)
  @JsonNotification("initialized")
  def initialized(params: InitializedParams): CompletableFuture[Unit] = {
    // Avoid duplicate `initialized` notifications. During the transition
    // for https://github.com/natebosch/vim-lsc/issues/113 to get fixed,
    // we may have users on a fixed vim-lsc version but with -Dmetals.no-initialized=true
    // enabled.
    if (isInitialized.compareAndSet(false, true)) {
      statusBar.start(sh, 0, 1, TimeUnit.SECONDS)
      tables.connect()
      registerNiceToHaveFilePatterns()
      val result = Future
        .sequence(
          List[Future[Unit]](
            buildServerManager.quickConnectToBuildServer().ignoreValue,
            buildServerManager
              .slowConnectToBuildServer(forceImport = false)
              .ignoreValue,
            Future(workspaceSymbols.indexClasspath()),
            Future(startHttpServer()),
            Future(formattingProvider.load())
          )
        )
        .ignoreValue
      result
    } else {
      scribe.warn("Ignoring duplicate 'initialized' notification.")
      Future.successful(())
    }
  }.recover { case NonFatal(e) =>
    scribe.error("Unexpected error initializing server", e)
  }.asJava

  lazy val shutdownPromise = new AtomicReference[Promise[Unit]](null)
  @JsonRequest("shutdown")
  def shutdown(): CompletableFuture[Unit] = {
    val promise = Promise[Unit]()
    // Ensure we only run `shutdown` at most once and that `exit` waits for the
    // `shutdown` promise to complete.
    if (shutdownPromise.compareAndSet(null, promise)) {
      scribe.info("shutting down Metals")
      try {
        cancel()
      } catch {
        case NonFatal(e) =>
          scribe.error("cancellation error", e)
      } finally {
        promise.success(())
      }
      if (clientConfig.isExitOnShutdown) {
        System.exit(0)
      }
      promise.future.asJava
    } else {
      shutdownPromise.get().future.asJava
    }
  }

  @JsonNotification("exit")
  def exit(): Unit = {
    // `shutdown` is idempotent, we can trigger it as often as we like.
    shutdown()
    // Ensure that `shutdown` has completed before killing the process.
    // Some clients may send `exit` immediately after `shutdown` causing
    // the build server to get killed before it can clean up resources.
    try {
      Await.result(
        shutdownPromise.get().future,
        Duration(3, TimeUnit.SECONDS)
      )
    } catch {
      case NonFatal(e) =>
        scribe.error("shutdown error", e)
    } finally {
      System.exit(0)
    }
  }

  private val didOpenHandler = DidOpenHandler(
    () => focusedDocument,
    p => { focusedDocument = p },
    openedFiles,
    recentlyFocusedFiles,
    fingerprints,
    packageProvider,
    buffers,
    languageClient,
    interactiveSemanticdbs,
    ammonite,
    compilers,
    compilations,
    parseTrees,
    charset,
    executionContext,
    syntheticsDecorator,
    () => workspace
  )

  @JsonNotification("textDocument/didOpen")
  def didOpen(params: DidOpenTextDocumentParams): CompletableFuture[Unit] =
    didOpenHandler(params)

  private val metalsDidFocusTextDocumentHandler =
    MetalsDidFocusTextDocumentHandler(
      path => { focusedDocument = Some(path) },
      buildTargets,
      focusedDocumentBuildTarget,
      interactiveSemanticdbs,
      () => workspace,
      openedFiles,
      syntheticsDecorator,
      worksheetProvider,
      compilations,
      executionContext
    )

  @JsonNotification("metals/didFocusTextDocument")
  def didFocus(
      params: AnyRef
  ): CompletableFuture[DidFocusResult.Value] =
    metalsDidFocusTextDocumentHandler(params)

  @JsonNotification("metals/windowStateDidChange")
  def windowStateDidChange(params: WindowStateDidChangeParams): Unit = {
    if (params.focused) {
      pauseables.unpause()
    } else {
      pauseables.pause()
    }
  }

  @JsonNotification("textDocument/didChange")
  def didChange(
      params: DidChangeTextDocumentParams
  ): CompletableFuture[Unit] =
    params.getContentChanges.asScala.headOption match {
      case None => CompletableFuture.completedFuture(())
      case Some(change) =>
        val path = params.getTextDocument.getUri.toAbsolutePath
        buffers.put(path, change.getText)
        diagnostics.didChange(path)
        parseTrees(path)
          .flatMap { _ => syntheticsDecorator.publishSynthetics(path) }
          .ignoreValue
          .asJava
    }

  @JsonNotification("textDocument/didClose")
  def didClose(params: DidCloseTextDocumentParams): Unit = {
    val path = params.getTextDocument.getUri.toAbsolutePath
    if (focusedDocument.contains(path)) {
      focusedDocument = recentlyFocusedFiles.pollRecent()
    }
    buffers.remove(path)
    compilers.didClose(path)
    trees.didClose(path)
    diagnostics.onNoSyntaxError(path)
  }

  @JsonNotification("textDocument/didSave")
  def didSave(params: DidSaveTextDocumentParams): CompletableFuture[Unit] = {
    val path = params.getTextDocument.getUri.toAbsolutePath
    savedFiles.add(path)
    // read file from disk, we only remove files from buffers on didClose.
    buffers.put(path, path.toInput.text)
    Future
      .sequence(
        List(
          Future(renameProvider.runSave()),
          parseTrees(path),
          onChange(List(path))
        )
      )
      .ignoreValue
      .asJava
  }

  private def didCompileTarget(report: CompileReport): Unit = {
    if (!isReliableFileWatcher) {
      // NOTE(olafur) this step is exclusively used when running tests on
      // non-Linux computers to avoid flaky failures caused by delayed file
      // watching notifications. The SemanticDB indexer depends on file watching
      // notifications to pick up `*.semanticdb` file updates and there's no
      // reliable way to await until those notifications appear.
      for {
        item <- buildTargets.scalacOptions(report.getTarget())
        scalaInfo <- buildTargets.scalaInfo(report.getTarget)
        semanticdb =
          item
            .targetroot(scalaInfo.getScalaVersion)
            .resolve(Directories.semanticdb)
        generatedFile <- semanticdb.listRecursive
      } {
        val event =
          new DirectoryChangeEvent(EventType.MODIFY, generatedFile.toNIO, 1)
        didChangeWatchedFiles(event).get()
      }
    }
  }

  private val workspaceDidChangeConfigurationHandler =
    WorkspaceDidChangeConfigurationHandler(
      executionContext,
      () => userConfig,
      userConfig0 => { userConfig = userConfig0 },
      ammonite,
      excludedPackageHandler,
      workspaceSymbols,
      languageClient,
      compilers,
      syntheticsDecorator,
      () => bspSession,
      bloopServers,
      buildServerManager,
      buildTargets
    )

  @JsonNotification("workspace/didChangeConfiguration")
  def didChangeConfiguration(
      params: DidChangeConfigurationParams
  ): CompletableFuture[Unit] =
    workspaceDidChangeConfigurationHandler(params)

  @JsonNotification("workspace/didChangeWatchedFiles")
  def didChangeWatchedFiles(
      params: DidChangeWatchedFilesParams
  ): CompletableFuture[Unit] = {
    val paths = params.getChanges.asScala.iterator
      .map(_.getUri.toAbsolutePath)
      .filterNot(savedFiles.isRecentlyActive) // de-duplicate didSave events.
      .toSeq
    onChange(paths).asJava
  }

  // This method is run the FileWatcher, so it should not do anything expensive on the main thread
  private def didChangeWatchedFiles(
      event: DirectoryChangeEvent
  ): CompletableFuture[Unit] = {
    if (event.eventType() == EventType.OVERFLOW && event.path() == null) {
      Future {
        semanticDBIndexer.onOverflow()
      }.asJava
    } else {
      val path = AbsolutePath(event.path())
      val isScalaOrJava = path.isScalaOrJava
      if (isScalaOrJava && event.eventType() == EventType.DELETE) {
        Future {
          diagnostics.didDelete(path)
        }.asJava
      } else if (
        isScalaOrJava && !savedFiles.isRecentlyActive(path) && !buffers
          .contains(path)
      ) {
        event.eventType() match {
          case EventType.CREATE =>
            mainBuildTargetsData.onCreate(path)
          case _ =>
        }
        onChange(List(path)).asJava
      } else if (path.isSemanticdb) {
        Future {
          event.eventType() match {
            case EventType.DELETE =>
              semanticDBIndexer.onDelete(event.path())
            case EventType.CREATE | EventType.MODIFY =>
              semanticDBIndexer.onChange(event.path())
            case EventType.OVERFLOW =>
              semanticDBIndexer.onOverflow(event.path())
          }
        }.asJava
      } else if (path.isBuild) {
        onBuildChanged(List(path)).ignoreValue.asJava
      } else {
        CompletableFuture.completedFuture(())
      }
    }
  }

  private def onChange(paths: Seq[AbsolutePath]): Future[Unit] = {
    paths.foreach { path =>
      fingerprints.add(path, FileIO.slurp(path, charset))
    }
    Future
      .sequence(
        List(
          Future(indexer.reindexWorkspaceSources(paths)),
          compilations.compileFiles(paths),
          onBuildChanged(paths).ignoreValue
        ) ++ paths.map(f => Future(interactiveSemanticdbs.textDocument(f)))
      )
      .ignoreValue
  }

  val textDocumentReferencesHandler = TextDocumentReferencesHandler(
    time,
    referencesProvider,
    clientConfig,
    compilations,
    buffers,
    trees,
    statusBar,
    executionContext
  )

  val textDocumentDefinitionHandler = TextDocumentDefinitionHandler(
    semanticdbs,
    executionContext,
    definitionProvider,
    textDocumentReferencesHandler,
    warnings,
    timerProvider,
    clientConfig,
    interactiveSemanticdbs
  )

  @JsonRequest("textDocument/definition")
  def definition(
      position: TextDocumentPositionParams
  ): CompletableFuture[util.List[Location]] =
    textDocumentDefinitionHandler(position)

  @JsonRequest("textDocument/typeDefinition")
  def typeDefinition(
      position: TextDocumentPositionParams
  ): CompletableFuture[util.List[Location]] =
    CancelTokens { _ =>
      scribe.warn("textDocument/typeDefinition is not supported.")
      null
    }

  @JsonRequest("textDocument/implementation")
  def implementation(
      position: TextDocumentPositionParams
  ): CompletableFuture[util.List[Location]] =
    CancelTokens { _ =>
      implementationProvider.implementations(position).asJava
    }

  private val textDocumentHoverHandler = TextDocumentHoverHandler(
    compilers,
    syntheticsDecorator,
    executionContext,
    worksheetProvider
  )

  @JsonRequest("textDocument/hover")
  def hover(params: TextDocumentPositionParams): CompletableFuture[Hover] =
    textDocumentHoverHandler(params)

  @JsonRequest("textDocument/documentHighlight")
  def documentHighlights(
      params: TextDocumentPositionParams
  ): CompletableFuture[util.List[DocumentHighlight]] =
    CancelTokens { _ => documentHighlightProvider.documentHighlight(params) }

  @JsonRequest("textDocument/documentSymbol")
  def documentSymbol(
      params: DocumentSymbolParams
  ): CompletableFuture[
    JEither[util.List[DocumentSymbol], util.List[SymbolInformation]]
  ] =
    CancelTokens { _ =>
      documentSymbolProvider
        .documentSymbols(params.getTextDocument().getUri().toAbsolutePath)
        .asJava
    }

  @JsonRequest("textDocument/formatting")
  def formatting(
      params: DocumentFormattingParams
  ): CompletableFuture[util.List[TextEdit]] =
    CancelTokens.future { token =>
      formattingProvider.format(
        params.getTextDocument.getUri.toAbsolutePath,
        token
      )
    }

  @JsonRequest("textDocument/onTypeFormatting")
  def onTypeFormatting(
      params: DocumentOnTypeFormattingParams
  ): CompletableFuture[util.List[TextEdit]] =
    CancelTokens { _ =>
      multilineStringFormattingProvider.format(params).asJava
    }

  @JsonRequest("textDocument/rangeFormatting")
  def rangeFormatting(
      params: DocumentRangeFormattingParams
  ): CompletableFuture[util.List[TextEdit]] =
    CancelTokens { _ =>
      multilineStringFormattingProvider.format(params).asJava
    }

  @JsonRequest("textDocument/prepareRename")
  def prepareRename(
      params: TextDocumentPositionParams
  ): CompletableFuture[l.Range] =
    CancelTokens.future { token =>
      renameProvider.prepareRename(params, token).map(_.orNull)
    }

  @JsonRequest("textDocument/rename")
  def rename(
      params: RenameParams
  ): CompletableFuture[WorkspaceEdit] =
    CancelTokens.future { token => renameProvider.rename(params, token) }

  @JsonRequest("textDocument/references")
  def references(
      params: ReferenceParams
  ): CompletableFuture[util.List[Location]] =
    textDocumentReferencesHandler(params)

  @JsonRequest("textDocument/completion")
  def completion(params: CompletionParams): CompletableFuture[CompletionList] =
    CancelTokens.future { token => compilers.completions(params, token) }

  @JsonRequest("completionItem/resolve")
  def completionItemResolve(
      item: CompletionItem
  ): CompletableFuture[CompletionItem] =
    CancelTokens.future { token =>
      if (clientConfig.isCompletionItemResolve) {
        compilers.completionItemResolve(item, token)
      } else {
        Future.successful(item)
      }
    }

  @JsonRequest("textDocument/signatureHelp")
  def signatureHelp(
      params: TextDocumentPositionParams
  ): CompletableFuture[SignatureHelp] =
    CancelTokens.future { token =>
      compilers.signatureHelp(params, token)
    }

  @JsonRequest("textDocument/codeAction")
  def codeAction(
      params: CodeActionParams
  ): CompletableFuture[util.List[l.CodeAction]] =
    CancelTokens.future { token =>
      codeActionProvider.codeActions(params, token).map(_.asJava)
    }

  @JsonRequest("textDocument/codeLens")
  def codeLens(
      params: CodeLensParams
  ): CompletableFuture[util.List[CodeLens]] =
    CancelTokens { _ =>
      timerProvider.timedThunk(
        "code lens generation",
        thresholdMillis = 1.second.toMillis
      ) {
        val path = params.getTextDocument.getUri.toAbsolutePath
        codeLensProvider.findLenses(path).toList.asJava
      }
    }

  @JsonRequest("textDocument/foldingRange")
  def foldingRange(
      params: FoldingRangeRequestParams
  ): CompletableFuture[util.List[FoldingRange]] = {
    CancelTokens.future { token =>
      parseTrees.currentFuture.map(_ =>
        foldingRangeProvider.getRangedFor(
          params.getTextDocument().getUri().toAbsolutePath
        )
      )
    }
  }

  @JsonRequest("textDocument/selectionRange")
  def selectionRange(
      params: SelectionRangeParams
  ): CompletableFuture[util.List[SelectionRange]] = {
    CancelTokens.future { token =>
      compilers.selectionRange(params, token)
    }
  }

  private val workspaceSymbolHandler = WorkspaceSymbolHandler(
    indexingPromise,
    time,
    workspaceSymbols,
    clientConfig,
    executionContext
  )

  @JsonRequest("workspace/symbol")
  def workspaceSymbol(
      params: WorkspaceSymbolParams
  ): CompletableFuture[util.List[SymbolInformation]] =
    workspaceSymbolHandler(params)

  def workspaceSymbol(query: String): Seq[SymbolInformation] = {
    workspaceSymbols.search(query)
  }

  private val executeCommandHandler = ExecuteCommandHandler(
    indexer.indexWorkspaceSources,
    bloopServers,
    buildServerManager,
    executionContext,
    () => bspSession,
    doctor,
    bspConnector,
    compilations,
    () => workspace,
    buffers,
    compilers,
    languageClient,
    definitionProvider,
    () => focusedDocument,
    debugProvider,
    scalaVersionSelector,
    cancelables,
    stacktraceAnalyzer,
    supermethods,
    popupChoiceReset,
    newFileProvider,
    ammonite,
    newProjectProvider,
    worksheetProvider,
    codeActionProvider,
    buildTools,
    tables,
    bspConfigGenerator
  )

  @JsonRequest("workspace/executeCommand")
  def executeCommand(
      params: ExecuteCommandParams
  ): CompletableFuture[Object] =
    executeCommandHandler(params)

  @JsonRequest("metals/treeViewChildren")
  def treeViewChildren(
      params: TreeViewChildrenParams
  ): CompletableFuture[MetalsTreeViewChildrenResult] = {
    Future {
      treeView.children(params)
    }.asJava
  }

  @JsonRequest("metals/treeViewParent")
  def treeViewParent(
      params: TreeViewParentParams
  ): CompletableFuture[TreeViewParentResult] = {
    Future {
      treeView.parent(params)
    }.asJava
  }

  @JsonNotification("metals/treeViewVisibilityDidChange")
  def treeViewVisibilityDidChange(
      params: TreeViewVisibilityDidChangeParams
  ): CompletableFuture[Unit] =
    Future {
      treeView.onVisibilityDidChange(params)
    }.asJava

  @JsonNotification("metals/treeViewNodeCollapseDidChange")
  def treeViewNodeCollapseDidChange(
      params: TreeViewNodeCollapseDidChangeParams
  ): CompletableFuture[Unit] =
    Future {
      treeView.onCollapseDidChange(params)
    }.asJava

  @JsonRequest("metals/treeViewReveal")
  def treeViewReveal(
      params: TextDocumentPositionParams
  ): CompletableFuture[TreeViewNodeRevealResult] =
    Future {
      treeView
        .reveal(
          params.getTextDocument().getUri().toAbsolutePath,
          params.getPosition()
        )
        .orNull
    }.asJava

  private val indexer = scala.meta.ls.Indexer(
    workspaceReload,
    doctor,
    languageClient,
    () => bspSession,
    executionContext,
    tables,
    statusBar,
    timerProvider,
    scalafixProvider,
    indexingPromise,
    ammonite,
    () => buildServerManager.lastImportedBuilds,
    clientConfig,
    definitionIndex,
    referencesProvider,
    workspaceSymbols,
    buildTargets,
    mainBuildTargetsData,
    interactiveSemanticdbs,
    buildClient,
    semanticDBIndexer,
    () => treeView,
    worksheetProvider,
    symbolSearch,
    buildTools,
    formattingProvider,
    fileWatcher,
    () => focusedDocument,
    focusedDocumentBuildTarget,
    buildTargetClasses,
    () => userConfig,
    sh,
    symbolDocs,
    scalaVersionSelector,
    sourceMapper
  )

  private val buildServerManager: scala.meta.ls.BuildServerManager =
    new scala.meta.ls.BuildServerManager(
      tables,
      languageClient,
      buildTools,
      warnings,
      buildToolSelector,
      executionContext,
      () => workspace,
      indexer,
      bloopInstall,
      buildServerPromise,
      compilations,
      buffers,
      compilers,
      timerProvider,
      bspConnector,
      () => userConfig,
      () => treeView,
      () => bspSession,
      sess => { bspSession = sess },
      statusBar,
      doctor,
      mainBuildTargetsData,
      diagnostics,
      bloopServers
    )

  private def onWorksheetChanged(
      paths: Seq[AbsolutePath]
  ): Future[Unit] = {
    paths
      .find { path =>
        if (clientConfig.isDidFocusProvider || focusedDocument.isDefined) {
          focusedDocument.contains(path) &&
          path.isWorksheet
        } else {
          path.isWorksheet
        }
      }
      .fold(Future.successful(()))(
        worksheetProvider.evaluateAndPublish(_, EmptyCancelToken)
      )
  }

  private def onBuildChangedUnbatched(
      paths: Seq[AbsolutePath]
  ): Future[BuildChange] = {
    val isBuildChange = paths.exists(buildTools.isBuildRelated(workspace, _))
    if (isBuildChange) {
      buildServerManager.slowConnectToBuildServer(forceImport = false)
    } else {
      Future.successful(BuildChange.None)
    }
  }

  private def newSymbolIndex(): OnDemandSymbolIndex = {
    OnDemandSymbolIndex.empty(
      onError = {
        case e @ (_: ParseException | _: TokenizeException) =>
          scribe.error(e.toString)
        case e: InvalidJarException =>
          scribe.warn(s"invalid jar: ${e.path}")
        case _: NoSuchFileException =>
        // only comes for badly configured jar with `/Users` path added.
        case NonFatal(e) =>
          scribe.error("unexpected error during source scanning", e)
      },
      toIndexSource = sourceMapper.actualSource
    )
  }

}
