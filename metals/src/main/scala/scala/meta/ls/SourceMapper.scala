package scala.meta.ls

import scala.meta.io.AbsolutePath
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.BuildTargets
import scala.meta.inputs.Input
import scala.meta.internal.metals.AdjustLspData
import org.eclipse.{lsp4j => l}
import scala.meta.internal.builds.SbtBuildTool
import scala.meta.internal.metals.ScalaVersions
import scala.meta.internal.worksheets.WorksheetProvider
import scala.meta.internal.metals.Buffers
import scala.meta.internal.metals.AdjustedLspData

final case class SourceMapper(
    buildTargets: BuildTargets,
    buffers: Buffers,
    workspace: () => AbsolutePath
) {

  def actualSource(path: AbsolutePath): AbsolutePath =
    buildTargets.actualSource(path).map(_.path).getOrElse(path)

  def actualSourceForPc(
      path: AbsolutePath,
      position: l.Position,
      scalaVersion: String
  ): (Input.VirtualFile, l.Position, AdjustLspData) = {

    def input = path.toInputFromBuffers(buffers)
    def default = {
      val viaBuildTargets =
        buildTargets.actualSource(path).map(_.update(input.value, position))
      viaBuildTargets.getOrElse((input, position, AdjustedLspData.default))
    }

    val forScripts =
      if (path.isSbt) {
        buildTargets
          .sbtAutoImports(path)
          .map(
            SbtBuildTool.sbtInputPosAdjustment(input, _, position)
          )
      } else if (
        path.isWorksheet && ScalaVersions.isScala3Version(scalaVersion)
      ) {
        WorksheetProvider.worksheetScala3Adjustments(input, path, position)
      } else None

    forScripts.getOrElse(default)
  }
}
