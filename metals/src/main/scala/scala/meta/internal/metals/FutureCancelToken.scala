package scala.meta.internal.metals

import java.lang
import java.util.concurrent.CompletionStage

import scala.compat.java8.FutureConverters._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success

import scala.meta.pc.CancelToken

/**
 * A cancel token backed by a Scala future.
 */
case class FutureCancelToken(f: Future[Boolean])(ec: ExecutionContext)
    extends CancelToken {
  var isCancelled: Boolean = false
  f.onComplete({
    case Failure(_) =>
      isCancelled = true
    case Success(cancel) =>
      isCancelled = cancel
  })(ec)

  override def checkCanceled(): Unit = {
    if (isCancelled) {
      throw new InterruptedException()
    }
  }

  override def onCancel(): CompletionStage[lang.Boolean] =
    f.map(cancel => java.lang.Boolean.valueOf(cancel))(ec).toJava
}

object FutureCancelToken {
  def fromUnit(
      f: Future[Unit]
  )(ec: ExecutionContext): FutureCancelToken =
    FutureCancelToken(f.map(_ => true)(ec))(ec)
}
