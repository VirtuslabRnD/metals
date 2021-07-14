package scala.meta.ls.handlers

import org.eclipse.lsp4j.DidChangeConfigurationParams
import scala.concurrent.Future
import java.util.concurrent.CompletableFuture
import scala.meta.internal.metals.UserConfiguration
import scala.meta.internal.metals.ScalaVersions
import scala.meta.internal.metals.Messages
import scala.meta.internal.metals.ammonite.Ammonite
import scala.concurrent.ExecutionContextExecutorService
import com.google.gson.JsonElement
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.ExcludedPackagesHandler
import scala.meta.internal.metals.WorkspaceSymbolProvider
import scala.meta.internal.metals.DelegatingLanguageClient
import scala.meta.internal.metals.Compilers
import scala.meta.internal.decorations.SyntheticsDecorationProvider
import scala.meta.internal.bsp.BspSession
import scala.meta.internal.metals.BloopServers
import scala.meta.internal.metals.BuildTargets

final case class WorkspaceDidChangeConfigurationHandler(
    executionContext: ExecutionContextExecutorService,
    userConfig: () => UserConfiguration,
    setUserConfig: UserConfiguration => Unit,
    ammonite: Ammonite,
    excludedPackageHandler: ExcludedPackagesHandler,
    workspaceSymbols: WorkspaceSymbolProvider,
    languageClient: DelegatingLanguageClient,
    compilers: Compilers,
    syntheticsDecorator: SyntheticsDecorationProvider,
    bspSession: () => Option[BspSession],
    bloopServers: BloopServers,
    buildServerManager: scala.meta.ls.BuildServerManager,
    buildTargets: BuildTargets
) {

  private implicit def ec = executionContext

  def apply(params: DidChangeConfigurationParams): CompletableFuture[Unit] =
    Future {
      val json = params.getSettings.asInstanceOf[JsonElement].getAsJsonObject
      UserConfiguration.fromJson(json) match {
        case Left(errors) =>
          errors.foreach { error => scribe.error(s"config error: $error") }
          Future.successful(())
        case Right(newUserConfig) =>
          val old = userConfig()
          setUserConfig(newUserConfig)
          if (newUserConfig.excludedPackages != old.excludedPackages) {
            excludedPackageHandler.update(newUserConfig.excludedPackages)
            workspaceSymbols.indexClasspath()
          }

          newUserConfig.fallbackScalaVersion.foreach { version =>
            if (!ScalaVersions.isSupportedScalaVersion(version)) {
              val params =
                Messages.UnsupportedScalaVersion.fallbackScalaVersionParams(
                  version
                )
              languageClient.showMessage(params)
            }
          }

          if (newUserConfig.symbolPrefixes != old.symbolPrefixes) {
            compilers.restartAll()
          }

          if (
            newUserConfig.showImplicitArguments != old.showImplicitArguments ||
            newUserConfig.showImplicitConversionsAndClasses != old.showImplicitConversionsAndClasses ||
            newUserConfig.showInferredType != old.showInferredType
          ) {
            syntheticsDecorator.refresh()
          }

          bspSession()
            .map { session =>
              if (session.main.isBloop) {
                bloopServers.ensureDesiredVersion(
                  newUserConfig.currentBloopVersion,
                  session.version,
                  newUserConfig.bloopVersion.nonEmpty,
                  old.bloopVersion.isDefined,
                  () => buildServerManager.autoConnectToBuildServer
                )
              } else if (
                newUserConfig.ammoniteJvmProperties != old.ammoniteJvmProperties && buildTargets.allBuildTargetIds
                  .exists(Ammonite.isAmmBuildTarget)
              ) {
                languageClient
                  .showMessageRequest(
                    Messages.AmmoniteJvmParametersChange.params()
                  )
                  .asScala
                  .flatMap {
                    case item
                        if item == Messages.AmmoniteJvmParametersChange.restart =>
                      ammonite.reload()
                    case _ =>
                      Future.successful(())
                  }
              } else {
                Future.successful(())
              }
            }
            .getOrElse(Future.successful(()))
      }
    }.flatten.asJava

}
