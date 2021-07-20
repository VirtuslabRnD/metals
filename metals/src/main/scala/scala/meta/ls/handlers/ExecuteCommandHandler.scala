package scala.meta.ls.handlers

import java.util.concurrent.CompletableFuture
import scala.collection.mutable
import org.eclipse.lsp4j.TextDocumentPositionParams
import scala.meta.internal.metals.Argument
import org.eclipse.{lsp4j => l}
import scala.meta.internal.metals.ServerCommands
import scala.concurrent.Future
import scala.meta.internal.metals.Urls
import scala.meta.internal.metals.ClientCommands
import scala.meta.internal.metals.Directories
import scala.meta.internal.metals.BloopServers
import scala.meta.internal.metals.MetalsEnrichments._
import scala.concurrent.ExecutionContextExecutorService
import scala.meta.internal.bsp.BspSession
import scala.meta.internal.metals.Doctor
import scala.meta.internal.bsp.BspConnector
import scala.meta.internal.metals.Compilations
import scala.meta.io.AbsolutePath
import scala.meta.internal.metals.Buffers
import scala.meta.internal.metals.Compilers
import scala.meta.internal.metals.DelegatingLanguageClient
import scala.meta.internal.metals.DefinitionProvider
import scala.meta.internal.metals.debug.DebugProvider
import scala.meta.internal.metals.ScalaVersionSelector
import scala.meta.internal.metals.MutableCancelable
import scala.meta.internal.metals.DebugSession
import scala.meta.internal.metals.CancelTokens
import scala.meta.internal.metals.Messages
import scala.meta.internal.metals.codeactions.ExtractMemberDefinitionData
import com.google.gson.JsonPrimitive
import scala.meta.internal.metals.StacktraceAnalyzer
import scala.meta.internal.implementation.Supermethods
import scala.meta.internal.metals.PopupChoiceReset
import java.net.URI
import scala.meta.internal.metals.newScalaFile.NewFileProvider
import scala.meta.internal.metals.ammonite.Ammonite
import scala.meta.internal.builds.NewProjectProvider
import scala.meta.internal.worksheets.WorksheetProvider
import scala.meta.internal.metals.CodeActionProvider
import ch.epfl.scala.{bsp4j => b}
import scala.meta.internal.builds.BuildTool
import scala.meta.internal.bsp.BspConfigGenerationStatus
import scala.meta.internal.builds.BuildServerProvider
import scala.meta.internal.builds.BuildTools
import scala.meta.internal.metals.Tables
import scala.meta.internal.bsp.BspConfigGenerator
import scala.meta.ls.BuildServerManager
import scala.meta.internal.metals.scalacli.ScalaCli

final case class ExecuteCommandHandler(
    indexWorkspaceSources: () => Unit,
    bloopServers: BloopServers,
    buildServerManager: BuildServerManager,
    executionContext: ExecutionContextExecutorService,
    bspSession: () => Option[BspSession],
    doctor: Doctor,
    bspConnector: BspConnector,
    compilations: Compilations,
    workspace: () => AbsolutePath,
    buffers: Buffers,
    compilers: Compilers,
    languageClient: DelegatingLanguageClient,
    definitionProvider: DefinitionProvider,
    focusedDocument: () => Option[AbsolutePath],
    debugProvider: DebugProvider,
    scalaVersionSelector: ScalaVersionSelector,
    cancelables: MutableCancelable,
    stacktraceAnalyzer: StacktraceAnalyzer,
    supermethods: Supermethods,
    popupChoiceReset: PopupChoiceReset,
    newFileProvider: NewFileProvider,
    ammonite: Ammonite,
    scalaCli: ScalaCli,
    newProjectProvider: NewProjectProvider,
    worksheetProvider: WorksheetProvider,
    codeActionProvider: CodeActionProvider,
    buildTools: BuildTools,
    tables: Tables,
    bspConfigGenerator: BspConfigGenerator
) {

  private implicit def ec = executionContext

  def apply(params: l.ExecuteCommandParams): CompletableFuture[Object] = {
    def textDocumentPosition(
        args: mutable.Buffer[AnyRef]
    ): Option[(TextDocumentPositionParams, String)] = {
      for {
        arg0 <- args.lift(0)
        uri <- Argument.getAsString(arg0)
        arg1 <- args.lift(1)
        line <- Argument.getAsInt(arg1)
        arg2 <- args.lift(2)
        character <- Argument.getAsInt(arg2)
        pos = new l.Position(line, character)
        textDoc = new l.TextDocumentIdentifier(uri)
        params = new TextDocumentPositionParams(textDoc, pos)
      } yield (params, uri)
    }

    val command = Option(params.getCommand).getOrElse("")
    scribe.info(s"got command '$command'")
    command.stripPrefix("metals.") match {
      case ServerCommands.ScanWorkspaceSources() =>
        Future {
          indexWorkspaceSources()
        }.asJavaObject
      case ServerCommands.RestartBuildServer() =>
        bspSession().foreach { session =>
          if (session.main.isBloop) bloopServers.shutdownServer()
        }
        buildServerManager.autoConnectToBuildServer().asJavaObject
      case ServerCommands.GenerateBspConfig() =>
        generateBspConfig().asJavaObject
      case ServerCommands.ImportBuild() =>
        buildServerManager.slowConnectToBuildServer(true).asJavaObject
      case ServerCommands.ConnectBuildServer() =>
        buildServerManager.quickConnectToBuildServer().asJavaObject
      case ServerCommands.DisconnectBuildServer() =>
        buildServerManager.disconnectOldBuildServer().asJavaObject
      case ServerCommands.RunDoctor() =>
        Future {
          doctor.executeRunDoctor()
        }.asJavaObject
      case ServerCommands.BspSwitch() =>
        (for {
          isSwitched <- bspConnector.switchBuildServer(
            workspace(),
            () => buildServerManager.slowConnectToBuildServer(true)
          )
          _ <- {
            if (isSwitched) buildServerManager.quickConnectToBuildServer()
            else Future.successful(())
          }
        } yield ()).asJavaObject
      case ServerCommands.OpenBrowser(url) =>
        Future.successful(Urls.openBrowser(url)).asJavaObject
      case ServerCommands.CascadeCompile() =>
        compilations
          .cascadeCompileFiles(buffers.open.toSeq)
          .asJavaObject
      case ServerCommands.CleanCompile() =>
        compilations.recompileAll().asJavaObject
      case ServerCommands.CancelCompile() =>
        Future {
          compilations.cancel()
          scribe.info("compilation cancelled")
        }.asJavaObject
      case ServerCommands.PresentationCompilerRestart() =>
        Future {
          compilers.restartAll()
        }.asJavaObject
      case ServerCommands.GotoPosition() =>
        Future {
          // arguments are not checked but are of format:
          // singletonList(location: Location, otherWindow: Boolean)
          languageClient.metalsExecuteClientCommand(
            new l.ExecuteCommandParams(
              ClientCommands.GotoLocation.id,
              params.getArguments()
            )
          )
        }.asJavaObject

      case ServerCommands.GotoSymbol() =>
        Future {
          for {
            args <- Option(params.getArguments())
            argObject <- args.asScala.headOption
            symbol <- Argument.getAsString(argObject)
            location <- definitionProvider
              .fromSymbol(symbol, focusedDocument())
              .asScala
              .headOption
          } {
            languageClient.metalsExecuteClientCommand(
              new l.ExecuteCommandParams(
                ClientCommands.GotoLocation.id,
                List(location: Object).asJava
              )
            )
          }
        }.asJavaObject
      case ServerCommands.GotoLog() =>
        Future {
          val log = workspace().resolve(Directories.log)
          val linesCount = log.readText.linesIterator.size
          val pos = new l.Position(linesCount, 0)
          languageClient.metalsExecuteClientCommand(
            new l.ExecuteCommandParams(
              ClientCommands.GotoLocation.id,
              List(
                new l.Location(
                  log.toURI.toString(),
                  new l.Range(pos, pos)
                ): Object
              ).asJava
            )
          )
        }.asJavaObject
      case ServerCommands.StartDebugAdapter() =>
        val args = params.getArguments.asScala
        import scala.meta.internal.metals.debug.DebugParametersJsonParsers._
        val debugSessionParams: Future[b.DebugSessionParams] = args match {
          case Seq(debugSessionParamsParser.Jsonized(params))
              if params.getData != null =>
            Future.successful(params)
          case Seq(mainClassParamsParser.Jsonized(params))
              if params.mainClass != null =>
            debugProvider.resolveMainClassParams(params)
          case Seq(testClassParamsParser.Jsonized(params))
              if params.testClass != null =>
            debugProvider.resolveTestClassParams(params)
          case Seq(attachRemoteParamsParser.Jsonized(params))
              if params.hostName != null =>
            debugProvider.resolveAttachRemoteParams(params)
          case Seq(unresolvedParamsParser.Jsonized(params)) =>
            debugProvider.debugDiscovery(params)
          case _ =>
            val argExample = ServerCommands.StartDebugAdapter.arguments
            val msg = s"Invalid arguments: $args. Expecting: $argExample"
            Future.failed(new IllegalArgumentException(msg))
        }
        val session = for {
          params <- debugSessionParams
          server <- debugProvider.start(
            params,
            scalaVersionSelector
          )
        } yield {
          cancelables.add(server)
          DebugSession(server.sessionName, server.uri.toString)
        }
        session.asJavaObject

      case ServerCommands.AnalyzeStacktrace() =>
        Future {
          val command = stacktraceAnalyzer.analyzeCommand(params)
          command.foreach(languageClient.metalsExecuteClientCommand)
          scribe.debug(s"Executing AnalyzeStacktrace ${command}")
        }.asJavaObject

      case ServerCommands.GotoSuperMethod() =>
        Future {
          val command = supermethods.getGoToSuperMethodCommand(params)
          command.foreach(languageClient.metalsExecuteClientCommand)
          scribe.debug(s"Executing GoToSuperMethod ${command}")
        }.asJavaObject

      case ServerCommands.SuperMethodHierarchy() =>
        scribe.debug(s"Executing SuperMethodHierarchy ${command}")
        supermethods.jumpToSelectedSuperMethod(params).asJavaObject

      case ServerCommands.ResetChoicePopup() =>
        val argsMaybe = Option(params.getArguments())
        (argsMaybe.flatMap(_.asScala.headOption) match {
          case Some(arg: JsonPrimitive) =>
            val value = arg.getAsString().replace("+", " ")
            scribe.debug(
              s"Executing ResetChoicePopup ${command} for choice ${value}"
            )
            popupChoiceReset.reset(value)
          case _ =>
            scribe.debug(
              s"Executing ResetChoicePopup ${command} in interactive mode."
            )
            popupChoiceReset.interactiveReset()
        }).asJavaObject

      case ServerCommands.NewScalaFile() =>
        val args = params.getArguments.asScala
        val directoryURI =
          args.lift(0).flatMap(Argument.getAsString).map(new URI(_))
        val name = args.lift(1).flatMap(Argument.getAsString)
        val fileType = args.lift(2).flatMap(Argument.getAsString)
        newFileProvider
          .handleFileCreation(directoryURI, name, fileType)
          .asJavaObject

      case ServerCommands.StartAmmoniteBuildServer() =>
        ammonite.start().asJavaObject
      case ServerCommands.StopAmmoniteBuildServer() =>
        ammonite.stop()

      case ServerCommands.StartScalaCliServer() =>
        scribe.info("got scala-cli-start command")
        val f = focusedDocument().map(_.parent) match {
          case None => Future.unit
          case Some(newDir) =>
            val workspace0 = workspace()
            val updated =
              if (newDir.toNIO.startsWith(workspace0.toNIO)) {
                val relPath = workspace0.toNIO.relativize(newDir.toNIO)
                val segments =
                  relPath.iterator().asScala.map(_.toString).toVector
                val idx = segments.indexOf(".metals")
                if (idx < 0) newDir
                else
                  AbsolutePath(
                    segments.take(idx).foldLeft(workspace0.toNIO)(_.resolve(_))
                  )
              } else newDir
            if (scalaCli.roots.contains(updated)) Future.unit
            else scalaCli.start(scalaCli.roots :+ updated)
        }
        f.asJavaObject
      case ServerCommands.StopScalaCliServer() =>
        scalaCli.stop()

      case ServerCommands.NewScalaProject() =>
        newProjectProvider.createNewProjectFromTemplate().asJavaObject

      case ServerCommands.CopyWorksheetOutput() =>
        val args = params.getArguments.asScala
        val worksheet = args.lift(0).collect {
          case ws: JsonPrimitive if ws.isString =>
            ws.getAsString().toAbsolutePath
        }

        val output = worksheet.flatMap(worksheetProvider.copyWorksheetOutput(_))

        if (output.nonEmpty) {
          Future(output).asJavaObject
        } else {
          languageClient.showMessage(Messages.Worksheets.unableToExport)
          Future.successful(()).asJavaObject
        }

      case ServerCommands.InsertInferredType() =>
        CancelTokens.future { token =>
          val args = params.getArguments().asScala
          val futureOpt = textDocumentPosition(args).map { case (params, uri) =>
            for {
              edits <- compilers.insertInferredType(params, token)
              if (!edits.isEmpty())
              workspaceEdit = new l.WorkspaceEdit(Map(uri -> edits).asJava)
              _ <- languageClient
                .applyEdit(new l.ApplyWorkspaceEditParams(workspaceEdit))
                .asScala
            } yield ()
          }

          futureOpt.getOrElse {
            languageClient.showMessage(Messages.InsertInferredTypeFailed)
            Future.unit
          }.withObjectValue
        }(ec)
      case ServerCommands.ExtractMemberDefinition() =>
        CancelTokens.future { token =>
          val args = params.getArguments().asScala

          val futureOpt = for {
            (params, uri) <- textDocumentPosition(args)
          } yield {
            val data = ExtractMemberDefinitionData(uri, params)
            for {
              result <- codeActionProvider.executeCommands(data, token)
              future <- languageClient.applyEdit(result.edits).asScala
            } yield {
              result.goToLocation.foreach { location =>
                languageClient.metalsExecuteClientCommand(
                  new l.ExecuteCommandParams(
                    ClientCommands.GotoLocation.id,
                    List(location: Object).asJava
                  )
                )
              }
            }
          }

          futureOpt.getOrElse {
            Future(
              languageClient.showMessage(
                Messages.ExtractMemberDefinitionFailed
              )
            )(ec)
          }.withObjectValue
        }(ec)
      case cmd =>
        scribe.error(s"Unknown command '$cmd'")
        Future.successful(()).asJavaObject
    }
  }

  private def generateBspConfig(): Future[Unit] = {
    val servers: List[BuildTool with BuildServerProvider] =
      buildTools.loadSupported().collect {
        case buildTool: BuildServerProvider => buildTool
      }

    def ensureAndConnect(
        buildTool: BuildTool,
        status: BspConfigGenerationStatus.BspConfigGenerationStatus
    ): Unit =
      status match {
        case BspConfigGenerationStatus.Generated =>
          tables.buildServers.chooseServer(buildTool.executableName)
          buildServerManager.quickConnectToBuildServer().ignoreValue
        case BspConfigGenerationStatus.Cancelled => ()
        case BspConfigGenerationStatus.Failed(exit) =>
          exit match {
            case Left(exitCode) =>
              scribe.error(
                s"Create of .bsp failed with exit code: $exitCode"
              )
              languageClient.showMessage(
                Messages.BspProvider.genericUnableToCreateConfig
              )
            case Right(message) =>
              languageClient.showMessage(
                Messages.BspProvider.unableToCreateConfigFromMessage(
                  message
                )
              )
          }
      }

    (servers match {
      case Nil =>
        scribe.warn(Messages.BspProvider.noBuildToolFound.toString())
        languageClient.showMessage(Messages.BspProvider.noBuildToolFound)
        Future.successful(())
      case buildTool :: Nil =>
        buildTool
          .generateBspConfig(
            workspace(),
            languageClient,
            args =>
              bspConfigGenerator.runUnconditionally(
                buildTool,
                args
              )
          )
          .map(status => ensureAndConnect(buildTool, status))
      case buildTools =>
        bspConfigGenerator
          .chooseAndGenerate(buildTools)
          .map {
            case (
                  buildTool: BuildTool,
                  status: BspConfigGenerationStatus.BspConfigGenerationStatus
                ) =>
              ensureAndConnect(buildTool, status)
          }
    })
  }

}
