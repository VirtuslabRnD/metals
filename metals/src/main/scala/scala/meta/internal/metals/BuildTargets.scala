package scala.meta.internal.metals

import java.lang.{Iterable => JIterable}
import java.net.URLClassLoader
import java.util
import java.util.concurrent.ConcurrentLinkedQueue
import java.{util => ju}

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.control.NonFatal

import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.mtags.Symbol
import scala.meta.io.AbsolutePath

import ch.epfl.scala.bsp4j.BuildTarget
import ch.epfl.scala.bsp4j.BuildTargetIdentifier
import ch.epfl.scala.bsp4j.ScalaBuildTarget
import ch.epfl.scala.bsp4j.ScalacOptionsItem
import ch.epfl.scala.bsp4j.ScalacOptionsResult
import ch.epfl.scala.bsp4j.WorkspaceBuildTargetsResult
import org.eclipse.{lsp4j => l}
import scala.meta.inputs.Input

/**
 * In-memory cache for looking up build server metadata.
 */
final class BuildTargets(
    workspace: () => AbsolutePath,
    tables: Option[Tables]
) {
  private var data: BuildTargets.DataSeq =
    BuildTargets.DataSeq(BuildTargets.Data.create(), Nil)
  def allWritableData = data.writableDataIterator.toSeq

  val buildTargetsOrder: BuildTargetIdentifier => Int = {
    (t: BuildTargetIdentifier) =>
      var score = 1

      val isSupportedScalaVersion = scalaInfo(t).exists(t =>
        ScalaVersions.isSupportedScalaVersion(t.getScalaVersion())
      )
      if (isSupportedScalaVersion) score <<= 2

      val isJVM = scalacOptions(t).exists(_.isJVM)
      if (isJVM) score <<= 1

      // note(@tgodzik) once the support for Scala 3 is on par with Scala 2 this can be removed
      val isScala2 = scalaInfo(t).exists(info =>
        !ScalaVersions.isScala3Version(info.getScalaVersion())
      )
      if (isScala2) score <<= 1

      score
  }

  def sourceItems: Iterable[AbsolutePath] =
    data.tail.foldLeft(data.head.sourceItemsToBuildTarget.keys)(
      _ ++ _.sourceItemsToBuildTarget.keys
    )
  def sourceItemsToBuildTargets
      : Iterator[(AbsolutePath, JIterable[BuildTargetIdentifier])] =
    data.iterator.flatMap(_.sourceItemsToBuildTarget.iterator)
  def scalacOptions: Iterable[ScalacOptionsItem] =
    data.iterable.flatMap(_.scalacTargetInfo.values)

  def actualSource(path: AbsolutePath): Option[BuildTargets.MappedSource] =
    data.iterator
      .flatMap(_.actualSources.get(path).iterator)
      .toStream
      .headOption

  def allBuildTargetIds: Seq[BuildTargetIdentifier] =
    all.toSeq.map(_.info.getId())
  def all: Iterator[ScalaTarget] =
    data.iterator.flatMap(_.all)
  def allScalaTargets: Iterator[(BuildTargets.Data, ScalaTarget)] =
    data.iterator.flatMap(_.allScalaTargets)
  def scalaTarget(id: BuildTargetIdentifier): Option[ScalaTarget] =
    data.iterator.flatMap(_.scalaTarget(id).iterator).toStream.headOption

  def allWorkspaceJars: Iterator[AbsolutePath] = {
    val isVisited = new ju.HashSet[AbsolutePath]()
    Iterator(
      for {
        target <- all
        classpathEntry <- target.scalac.classpath
        if classpathEntry.isJar
        if isVisited.add(classpathEntry)
      } yield classpathEntry,
      PackageIndex.bootClasspath.iterator
    ).flatten
  }

  def buildTargetSources(
      id: BuildTargetIdentifier
  ): Iterable[AbsolutePath] =
    data.iterator
      .flatMap(_.buildTargetSources.get(id).iterator)
      .toStream
      .headOption
      .toSeq
      .flatMap(_.asScala)

  def buildTargetTransitiveSources(
      id: BuildTargetIdentifier
  ): Iterator[AbsolutePath] = {
    for {
      dependency <- buildTargetTransitiveDependencies(id).iterator
      sources <- data.iterator
        .flatMap(_.buildTargetSources.get(dependency).iterator)
        .take(1)
      source <- sources.asScala.iterator
    } yield source
  }

  def buildTargetTransitiveDependencies(
      id: BuildTargetIdentifier
  ): Iterable[BuildTargetIdentifier] = {
    val isVisited = mutable.Set.empty[BuildTargetIdentifier]
    val toVisit = new java.util.ArrayDeque[BuildTargetIdentifier]
    toVisit.add(id)
    while (!toVisit.isEmpty) {
      val next = toVisit.pop()
      if (!isVisited(next)) {
        isVisited.add(next)
        for {
          info <- info(next).iterator
          dependency <- info.getDependencies.asScala.iterator
        } {
          toVisit.add(dependency)
        }
      }
    }
    isVisited
  }

  def info(
      buildTarget: BuildTargetIdentifier
  ): Option[BuildTarget] =
    data.iterator
      .flatMap(_.buildTargetInfo.get(buildTarget).iterator)
      .toStream
      .headOption
  def scalaInfo(
      buildTarget: BuildTargetIdentifier
  ): Option[ScalaBuildTarget] =
    info(buildTarget).flatMap(_.asScalaBuildTarget)

  def scalacOptions(
      buildTarget: BuildTargetIdentifier
  ): Option[ScalacOptionsItem] =
    data.iterator
      .flatMap(_.scalacTargetInfo.get(buildTarget).iterator)
      .toStream
      .headOption

  def workspaceDirectory(
      buildTarget: BuildTargetIdentifier
  ): Option[AbsolutePath] =
    buildServerOf(buildTarget).map(_.workspaceDirectory)

  /**
   * Returns the first build target containing this source file.
   */
  def inverseSources(
      source: AbsolutePath
  ): Option[BuildTargetIdentifier] = {
    val buildTargets = sourceBuildTargets(source)
    val orSbtBuildTarget =
      buildTargets.getOrElse(sbtBuildScalaTarget(source).toIterable)
    if (orSbtBuildTarget.isEmpty) {
      tables
        .flatMap(_.dependencySources.getBuildTarget(source))
        .orElse(inferBuildTarget(source))
    } else {
      Some(orSbtBuildTarget.maxBy(buildTargetsOrder))
    }
  }

  /**
   * Resolves sbt auto imports if a file belongs to a Sbt build target.
   */
  def sbtAutoImports(path: AbsolutePath): Option[Seq[String]] =
    for {
      targetId <- inverseSources(path)
      target <- scalaTarget(targetId)
      imports <- target.autoImports
    } yield imports

  /**
   * Tries to guess what build target this readonly file belongs to from the symbols it defines.
   *
   * By default, we rely on carefully recording what build target produced what
   * files in the `.metals/readonly/` directory. This approach has the problem
   * that navigation failed to work in `readonly/` sources if
   *
   * - a new metals feature forgot to record the build target
   * - a user removes `.metals/metals.h2.db`
   *
   * When encountering an unknown `readonly/` file we do the following steps to
   * infer what build target it belongs to:
   *
   * - check if file is in `.metals/readonly/dependencies/${source-jar-name}`
   * - find the build targets that have a sourceDependency with that name
   *
   * Otherwise if it's a jar file we find a build target it belongs to.
   *
   * This approach is not glamorous but it seems to work reasonably well.
   */
  def inferBuildTarget(
      source: AbsolutePath
  ): Option[BuildTargetIdentifier] = {
    val readonly = workspace().resolve(Directories.readonly)
    source.toRelativeInside(readonly) match {
      case Some(rel) =>
        val names = rel.toNIO.iterator().asScala.toList.map(_.filename)
        names match {
          case Directories.dependenciesName :: jarName :: _ =>
            // match build target by source jar name
            data.iterator
              .flatMap(_.inverseDependencySources.iterator)
              .collectFirst {
                case (path, ids) if path.filename == jarName && ids.nonEmpty =>
                  ids.head
              }
          case _ =>
            None
        }
      case None =>
        // else it can be a source file inside a jar
        val fromJar = jarPath(source)
          .flatMap { jar =>
            allScalaTargets.find { case (_, scalaTarget) =>
              scalaTarget.jarClasspath.contains(jar)
            }
          }
        for ((data0, target) <- fromJar)
          data0.addSourceItem(source, target.id)
        fromJar.map(_._2.id)
    }
  }

  def findByDisplayName(name: String): Option[BuildTarget] = {
    data.iterator
      .flatMap(_.buildTargetInfo.valuesIterator)
      .find(_.getDisplayName() == name)
      .toStream
      .headOption
  }

  private def jarPath(source: AbsolutePath): Option[AbsolutePath] = {
    source.jarPath.map { sourceJarPath =>
      sourceJarPath.parent.resolve(
        source.filename.replace("-sources.jar", ".jar")
      )
    }
  }

  /**
   * Returns meta build target for `*.sbt` or `*.scala`  files.
   * It selects build target by directory of its connection
   *   because `*.sbt` and `*.scala` aren't included in `sourceFiles` set
   */
  def sbtBuildScalaTarget(
      file: AbsolutePath
  ): Option[BuildTargetIdentifier] = {
    val targetMetaBuildDir =
      if (file.isSbt) file.parent.resolve("project") else file.parent
    data.iterator
      .flatMap(_.buildTargetInfo.valuesIterator)
      .find { target =>
        val isMetaBuild = target.getDataKind == "sbt"
        if (isMetaBuild) {
          workspaceDirectory(target.getId)
            .map(_ == targetMetaBuildDir)
            .getOrElse(false)
        } else {
          false
        }
      }
      .map(_.getId())
      .toStream
      .headOption
  }

  case class InferredBuildTarget(
      jar: AbsolutePath,
      symbol: String,
      id: BuildTargetIdentifier
  )
  def inferBuildTarget(
      toplevels: Iterable[Symbol]
  ): Option[InferredBuildTarget] = {
    val classloader = new URLClassLoader(
      allWorkspaceJars.map(_.toNIO.toUri().toURL()).toArray,
      null
    )
    lazy val classpaths =
      all.map(i => i.id -> i.scalac.classpath.toSeq).toSeq
    try {
      toplevels.foldLeft(Option.empty[InferredBuildTarget]) {
        case (Some(x), _) => Some(x)
        case (None, toplevel) =>
          val classfile = toplevel.owner.value + toplevel.displayName + ".class"
          val resource = classloader
            .findResource(classfile)
            .toURI()
            .toString()
            .replaceFirst("!/.*", "")
            .stripPrefix("jar:")
          val path = resource.toAbsolutePath
          classpaths.collectFirst {
            case (id, classpath) if classpath.contains(path) =>
              InferredBuildTarget(path, toplevel.value, id)
          }
      }
    } catch {
      case NonFatal(_) =>
        None
    } finally {
      classloader.close()
    }
  }

  def sourceBuildTargets(
      sourceItem: AbsolutePath
  ): Option[Iterable[BuildTargetIdentifier]] =
    data.iterator
      .flatMap(_.sourceBuildTargets(sourceItem).iterator)
      .toStream
      .headOption

  def inverseSourceItem(source: AbsolutePath): Option[AbsolutePath] =
    sourceItems.find(item => source.toNIO.startsWith(item.toNIO))

  def originalInverseSourceItem(source: AbsolutePath): Option[AbsolutePath] =
    data.iterator
      .flatMap(_.originalSourceItems.asScala.iterator)
      .find(item => source.toNIO.startsWith(item.dealias.toNIO))

  def isInverseDependency(
      query: BuildTargetIdentifier,
      roots: List[BuildTargetIdentifier]
  ): Boolean = {
    BuildTargets.isInverseDependency(
      query,
      roots,
      id =>
        data.iterator
          .flatMap(_.inverseDependencies.get(id).iterator)
          .toStream
          .headOption
    )
  }
  def inverseDependencyLeaves(
      target: BuildTargetIdentifier
  ): collection.Set[BuildTargetIdentifier] = {
    computeInverseDependencies(target).leaves
  }
  def allInverseDependencies(
      target: BuildTargetIdentifier
  ): collection.Set[BuildTargetIdentifier] = {
    computeInverseDependencies(target).visited
  }
  private def computeInverseDependencies(
      target: BuildTargetIdentifier
  ): BuildTargets.InverseDependencies = {
    BuildTargets.inverseDependencies(
      List(target),
      id =>
        data.iterator
          .flatMap(_.inverseDependencies.get(id).iterator)
          .toStream
          .headOption
    )
  }

  def inverseDependencySource(
      sourceJar: AbsolutePath
  ): collection.Set[BuildTargetIdentifier] = {
    data.iterator
      .flatMap(_.inverseDependencySources.get(sourceJar).iterator)
      .toStream
      .headOption
      .getOrElse(Set.empty)
  }

  def sourceRoots: Iterable[AbsolutePath] = {
    data.iterable.flatMap(_.isSourceRoot.asScala)
  }

  def isInsideSourceRoot(path: AbsolutePath): Boolean = {
    data.iterator.exists(_.isSourceRoot.contains(path)) &&
    data.iterator.flatMap(_.isSourceRoot.asScala.iterator).exists { root =>
      path.toNIO.startsWith(root.toNIO)
    }
  }

  def buildServerOf(
      id: BuildTargetIdentifier
  ): Option[BuildServerConnection] =
    data.iterator
      .flatMap(_.targetToConnection.get(id).iterator)
      .toStream
      .headOption

  def addData(data: BuildTargets.WritableData): Unit =
    this.data = BuildTargets.DataSeq(data, this.data.head :: this.data.tail)
}

object BuildTargets {

  def isInverseDependency(
      query: BuildTargetIdentifier,
      roots: List[BuildTargetIdentifier],
      inverseDeps: BuildTargetIdentifier => Option[Seq[BuildTargetIdentifier]]
  ): Boolean = {
    val isVisited = mutable.Set.empty[BuildTargetIdentifier]
    @tailrec
    def loop(toVisit: List[BuildTargetIdentifier]): Boolean =
      toVisit match {
        case Nil => false
        case head :: tail =>
          if (head == query) true
          else if (isVisited(head)) false
          else {
            isVisited += head
            inverseDeps(head) match {
              case Some(next) =>
                loop(next.toList ++ tail)
              case None =>
                loop(tail)
            }
          }
      }
    loop(roots)
  }

  /**
   * Given an acyclic graph and a root target, returns the leaf nodes that depend on the root target.
   *
   * For example, returns `[D, E, C]` given the following graph with root A: {{{
   *      A
   *    ^   ^
   *    |   |
   *    B   C
   *   ^ ^
   *   | |
   *   D E
   * }}}
   */
  def inverseDependencies(
      root: List[BuildTargetIdentifier],
      inverseDeps: BuildTargetIdentifier => Option[Seq[BuildTargetIdentifier]]
  ): InverseDependencies = {
    val isVisited = mutable.Set.empty[BuildTargetIdentifier]
    val leaves = mutable.Set.empty[BuildTargetIdentifier]
    def loop(toVisit: List[BuildTargetIdentifier]): Unit =
      toVisit match {
        case Nil => ()
        case head :: tail =>
          if (!isVisited(head)) {
            isVisited += head
            inverseDeps(head) match {
              case Some(next) =>
                loop(next.toList)
              case None =>
                // Only add leaves of the tree to the result to minimize the number
                // of targets that we compile. If `B` depends on `A`, it's faster
                // in Bloop to compile only `B` than `A+B`.
                leaves += head
            }
            loop(tail)
          }
      }
    loop(root)
    InverseDependencies(isVisited, leaves)
  }

  case class InverseDependencies(
      visited: collection.Set[BuildTargetIdentifier],
      leaves: collection.Set[BuildTargetIdentifier]
  )

  trait Data {
    def sourceItemsToBuildTarget
        : scala.collection.Map[AbsolutePath, ConcurrentLinkedQueue[
          BuildTargetIdentifier
        ]]
    def buildTargetInfo
        : scala.collection.Map[BuildTargetIdentifier, BuildTarget]
    def scalacTargetInfo
        : scala.collection.Map[BuildTargetIdentifier, ScalacOptionsItem]
    def inverseDependencies
        : scala.collection.Map[BuildTargetIdentifier, ListBuffer[
          BuildTargetIdentifier
        ]]
    def buildTargetSources
        : scala.collection.Map[BuildTargetIdentifier, util.Set[AbsolutePath]]
    def inverseDependencySources
        : scala.collection.Map[AbsolutePath, Set[BuildTargetIdentifier]]
    def isSourceRoot: java.util.Set[AbsolutePath]
    // if workspace contains symlinks, original source items are kept here and source items dealiased
    def originalSourceItems: java.util.Set[AbsolutePath]

    def targetToConnection
        : scala.collection.Map[BuildTargetIdentifier, BuildServerConnection]
    def sourceBuildTargets(
        sourceItem: AbsolutePath
    ): Option[Iterable[BuildTargetIdentifier]]

    def actualSources
        : scala.collection.Map[AbsolutePath, BuildTargets.MappedSource]

    def all: Iterator[ScalaTarget]
    def allScalaTargets: Iterator[(BuildTargets.Data, ScalaTarget)]
    def scalaTarget(id: BuildTargetIdentifier): Option[ScalaTarget]

    def addSourceItem(
        sourceItem: AbsolutePath,
        buildTarget: BuildTargetIdentifier
    ): Unit
  }

  final case class DataSeq(head: WritableData, tail: List[WritableData]) {
    private lazy val list: List[Data] = head :: tail
    def iterator: Iterator[Data] = list.iterator
    def writableDataIterator: Iterator[WritableData] = (head :: tail).iterator
    def iterable: Iterable[Data] = list.toIterable
  }

  trait WritableData extends Data {
    def linkSourceFile(id: BuildTargetIdentifier, source: AbsolutePath): Unit
    def reset(): Unit
    def addWorkspaceBuildTargets(result: WorkspaceBuildTargetsResult): Unit
    def addScalacOptions(result: ScalacOptionsResult): Unit
    def addDependencySource(
        sourcesJar: AbsolutePath,
        target: BuildTargetIdentifier
    ): Unit
    def addMappedSource(path: AbsolutePath, mapped: MappedSource): Unit
    def resetConnections(
        idToConn: List[(BuildTargetIdentifier, BuildServerConnection)]
    ): Unit
    def onCreate(source: AbsolutePath): Unit
  }

  object Data {
    def create(): WritableData =
      new DataImpl
  }

  private final class DataImpl extends WritableData {

    val sourceItemsToBuildTarget =
      TrieMap.empty[AbsolutePath, ConcurrentLinkedQueue[BuildTargetIdentifier]]
    val buildTargetInfo =
      TrieMap.empty[BuildTargetIdentifier, BuildTarget]
    val scalacTargetInfo =
      TrieMap.empty[BuildTargetIdentifier, ScalacOptionsItem]
    val inverseDependencies =
      TrieMap.empty[BuildTargetIdentifier, ListBuffer[BuildTargetIdentifier]]
    val buildTargetSources =
      TrieMap.empty[BuildTargetIdentifier, util.Set[AbsolutePath]]
    val inverseDependencySources =
      TrieMap.empty[AbsolutePath, Set[BuildTargetIdentifier]]
    val isSourceRoot =
      ConcurrentHashSet.empty[AbsolutePath]
    // if workspace contains symlinks, original source items are kept here and source items dealiased
    val originalSourceItems = ConcurrentHashSet.empty[AbsolutePath]

    val targetToConnection =
      new mutable.HashMap[BuildTargetIdentifier, BuildServerConnection]

    private val sourceBuildTargetsCache =
      new util.concurrent.ConcurrentHashMap[AbsolutePath, Option[
        Iterable[BuildTargetIdentifier]
      ]]

    val actualSources =
      TrieMap.empty[AbsolutePath, BuildTargets.MappedSource]

    def reset(): Unit = {
      sourceItemsToBuildTarget.values.foreach(_.clear())
      sourceItemsToBuildTarget.clear()
      sourceBuildTargetsCache.clear()
      buildTargetInfo.clear()
      scalacTargetInfo.clear()
      inverseDependencies.clear()
      buildTargetSources.clear()
      inverseDependencySources.clear()
      isSourceRoot.clear()
    }

    def addSourceItem(
        sourceItem: AbsolutePath,
        buildTarget: BuildTargetIdentifier
    ): Unit = {
      val dealiased = sourceItem.dealias
      if (dealiased != sourceItem)
        originalSourceItems.add(sourceItem)

      val queue = sourceItemsToBuildTarget.getOrElseUpdate(
        dealiased,
        new ConcurrentLinkedQueue()
      )
      queue.add(buildTarget)
      sourceBuildTargetsCache.clear()
    }

    def linkSourceFile(
        id: BuildTargetIdentifier,
        source: AbsolutePath
    ): Unit = {
      val set = buildTargetSources.getOrElseUpdate(id, ConcurrentHashSet.empty)
      set.add(source)
    }

    def addWorkspaceBuildTargets(result: WorkspaceBuildTargetsResult): Unit = {
      result.getTargets.asScala.foreach { target =>
        buildTargetInfo(target.getId) = target
        target.getDependencies.asScala.foreach { dependency =>
          val buf =
            inverseDependencies.getOrElseUpdate(dependency, ListBuffer.empty)
          buf += target.getId
        }
      }
    }

    def addScalacOptions(result: ScalacOptionsResult): Unit = {
      result.getItems.asScala.foreach { item =>
        scalacTargetInfo(item.getTarget) = item
      }
    }

    def addDependencySource(
        sourcesJar: AbsolutePath,
        target: BuildTargetIdentifier
    ): Unit = {
      val acc = inverseDependencySources.getOrElse(sourcesJar, Set.empty)
      inverseDependencySources(sourcesJar) = acc + target
    }

    def addMappedSource(path: AbsolutePath, mapped: MappedSource): Unit =
      actualSources(path) = mapped

    def resetConnections(
        idToConn: List[(BuildTargetIdentifier, BuildServerConnection)]
    ): Unit = {
      targetToConnection.clear()
      idToConn.foreach { case (id, conn) => targetToConnection.put(id, conn) }
    }

    def onCreate(source: AbsolutePath): Unit =
      for (targets <- sourceBuildTargets(source); buildTarget <- targets)
        linkSourceFile(buildTarget, source)

    def sourceBuildTargets(
        sourceItem: AbsolutePath
    ): Option[Iterable[BuildTargetIdentifier]] = {
      val valueOrNull = sourceBuildTargetsCache.get(sourceItem)
      if (valueOrNull == null) {
        val value = sourceItemsToBuildTarget.collectFirst {
          case (source, buildTargets)
              if sourceItem.toNIO.getFileSystem == source.toNIO.getFileSystem &&
                sourceItem.toNIO.startsWith(source.toNIO) =>
            buildTargets.asScala
        }
        val prevOrNull = sourceBuildTargetsCache.putIfAbsent(sourceItem, value)
        if (prevOrNull == null) value
        else prevOrNull
      } else valueOrNull
    }

    def all: Iterator[ScalaTarget] =
      for {
        (_, target) <- buildTargetInfo.iterator
        scalaTarget <- toScalaTarget(target)
      } yield scalaTarget
    def allScalaTargets: Iterator[(BuildTargets.Data, ScalaTarget)] =
      for {
        (_, target) <- buildTargetInfo.iterator
        scalaTarget <- toScalaTarget(target)
      } yield (this, scalaTarget)
    def scalaTarget(id: BuildTargetIdentifier): Option[ScalaTarget] =
      for {
        target <- buildTargetInfo.get(id)
        scalaTarget <- toScalaTarget(target)
      } yield scalaTarget

    private def toScalaTarget(target: BuildTarget): Option[ScalaTarget] = {
      for {
        scalac <- scalacTargetInfo.get(target.getId)
        scalaTarget <- target.asScalaBuildTarget
      } yield {
        val autoImports = target.asSbtBuildTarget.map(_.getAutoImports.asScala)
        ScalaTarget(
          target,
          scalaTarget,
          scalac,
          autoImports,
          target.getDataKind() == "sbt"
        )
      }
    }

  }

  trait MappedSource {
    def path: AbsolutePath
    def update(
        content: String,
        pos: l.Position
    ): (Input.VirtualFile, l.Position, AdjustLspData)
  }

}
