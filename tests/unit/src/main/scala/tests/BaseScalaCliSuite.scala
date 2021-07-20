package tests

import scala.meta.internal.metals.Messages
import org.eclipse.lsp4j.MessageActionItem
import scala.concurrent.Promise
import scala.concurrent.Future
import scala.util.Success
import scala.util.Failure
import scala.concurrent.Await
import java.util.concurrent.Executors
import scala.concurrent.duration._
import java.util.concurrent.TimeoutException
import ch.epfl.scala.bsp4j.MessageType

abstract class BaseScalaCliSuite(scalaVersion: String)
    extends BaseLspSuite(s"scala-cli-$scalaVersion")
    with ScriptsAssertions {

  pprint.log(scalaVersion)

  private val scheduler = Executors.newSingleThreadScheduledExecutor()

  private def timeout(
      message: String,
      duration: FiniteDuration
  ): Future[Unit] = {
    val p = Promise[Unit]()
    val r: Runnable = { () =>
      p.failure(new TimeoutException(message))
    }
    scheduler.schedule(r, duration.length, duration.unit)
    p.future
  }

  override def afterAll(): Unit = {
    super.afterAll()
    scheduler.shutdown()
  }

  override def munitIgnore: Boolean =
    !isValidScalaVersionForEnv(scalaVersion)

  private val importedPromise = Promise[Unit]()

  override def newServer(workspaceName: String): Unit = {
    super.newServer(workspaceName)
    val previousShowMessageHandler = server.client.showMessageHandler
    server.client.showMessageHandler = { params =>
      if (params == Messages.ImportScalaCliProject.Imported)
        importedPromise.success(())
      else if (
        params.getType == MessageType.ERROR && params.getMessage.startsWith(
          "Error importing Scala CLI project "
        )
      )
        importedPromise.failure(
          new Exception(s"Error importing project: $params")
        )
      else
        previousShowMessageHandler(params)
    }
    val previousShowMessageRequestHandler =
      server.client.showMessageRequestHandler
    server.client.showMessageRequestHandler = { params =>
      pprint.log(params)
      if (params == Messages.ImportScalaCliProject.params())
        Some(new MessageActionItem(Messages.ImportScalaCliProject.importAll))
      else
        previousShowMessageRequestHandler(params)
    }
  }

  test("simple file") {
    val f = for {
      _ <- initialize(
          s"""
             |/metals.json
             |{
             |  "a": {
             |    "scalaVersion": "$scalaVersion"
             |  }
             |}
             |
             |/scala.conf
             |
             |/MyTests.scala
             |import $$ivy.`com.lihaoyi::utest::0.7.9`, utest._
             |import $$ivy.`com.lihaoyi::pprint::0.6.4`
             |
             |object MyTests extends TestSuite {
             |  pprint.log(2)
             |  val tests = Tests {
             |    test("foo") {
             |      assert(2 + 2 == 4)
             |    }
             |    test("nope") {
             |      assert(2 + 2 == 5)
             |    }
             |  }
             |}
             |""".stripMargin
        )
        .transform {
          case Success(()) => Success(())
          case Failure(ex) => Failure(new Exception(ex))
        }
      _ <- server.didOpen("MyTests.scala").transform {
        case Success(()) => Success(())
        case Failure(ex) => Failure(new Exception(ex))
      }

      _ <- Future
        .firstCompletedOf(
          List(
            importedPromise.future,
            timeout("import timeout", 20.seconds)
          )
        )
        .transform {
          case Success(()) => Success(())
          case Failure(ex) => Failure(new Exception(ex))
        }

      // via Scala CLI-generated Semantic DB
      _ <- assertDefinitionAtLocation(
        "MyTests.scala",
        "val tests = Test@@s",
        "utest/Tests.scala"
      )

      // via presentation compiler, using the Scala CLI build target classpath
      _ <- assertDefinitionAtLocation(
        "utest/Tests.scala",
        "import utest.framework.{TestCallTree, Tr@@ee}",
        "utest/framework/Tree.scala"
      )

    } yield ()

    val f0 = f.transform {
      case Success(()) => Success(())
      case Failure(ex) => Failure(new Exception(ex))
    }

    Await.result(f0, Duration.Inf)
  }

}
