package com.dadrox.scuttle.result

import com.dadrox.scuttle.Enum
import scala.concurrent.{ Future => ScalaFuture, _ }
import scala.util.{ Success => ScalaSuccess, Failure => ScalaFailure }
import scala.util.control.NonFatal
import com.dadrox.scuttle.time.{Time, Timer, Duration}
import java.util.concurrent.TimeoutException
import scala.concurrent.TimeoutException

object AwaitFailReason extends Enum {
    sealed case class EnumVal private[AwaitFailReason] (name: String) extends Value with Failure.Reason

    val Interrupted = EnumVal("Interrupted")
    val IllegalArgument = EnumVal("IllegalArgument")
    val Unknown = EnumVal("Unknown")
}
case class AwaitFailure(reason: AwaitFailReason.EnumVal) extends Failure.Detail {
    val message: String = reason.name
    val cause: Option[Throwable] = None
}

object TimeoutReason extends Enum {
    sealed case class EnumVal private[TimeoutReason] (name: String) extends Value with Failure.Reason

    val Await = EnumVal("Await")
    val Timer = EnumVal("Timer")
}
case class TimeoutFailure(reason: TimeoutReason.EnumVal, duration: Option[Duration]) extends Failure.Detail {
    val message: String = reason.name
    val cause: Option[Throwable] = None
}

trait Future[+T] {

    def underlying: ScalaFuture[Result[T]]

    final def flatMap[U](f: T => Future[U])(implicit executor: ExecutionContext): Future[U] = ConcreteFuture {
        underlying.flatMap {
            case Success(obj)     => f(obj).underlying
            case failure: Failure => ScalaFuture.successful(failure)
        }
    }

    final def map[U](f: T => U)(implicit executor: ExecutionContext): Future[U] = ConcreteFuture(underlying.map(_.map(f)))

    final def onSuccess[U](fn: T => U)(implicit executor: ExecutionContext): Future[T] = {
        underlying.onSuccess {
            case Success(s) => fn(s)
            case _          =>
        }
        this
    }

    final def onComplete[U](fn: Result[T] => U)(implicit executor: ExecutionContext): Future[T] = {
        underlying.onComplete {
            case ScalaSuccess(s) => fn(s)
            case _               =>
        }
        this
    }

    final def onFailure[U](fn: Failure.Detail => U)(implicit executor: ExecutionContext): Future[T] = {
        underlying.onSuccess {
            case Failure(f) => fn(f)
            case _          =>
        }
        this
    }

    def within(timeout: Duration)(implicit timer: Timer, executor: ExecutionContext): Future[T] = {
        val p = Promise[Result[T]]()
        timer.doAt(Time.now + timeout)(p success Failure(TimeoutFailure(TimeoutReason.Timer, Some(timeout))))
        ConcreteFuture(ScalaFuture.firstCompletedOf(List(underlying, p.future)))
    }

    def await(): Result[T] = await(Duration.fromDays(1))

    def await(atMost: Duration): Result[T] = {
        try Await.result(underlying, atMost.asScala())
        catch {
            case e: InterruptedException     => Failure(AwaitFailure(AwaitFailReason.Interrupted))
            case e: TimeoutException         => Failure(TimeoutFailure(TimeoutReason.Await, Some(atMost)))
            case e: IllegalArgumentException => Failure(AwaitFailure(AwaitFailReason.IllegalArgument))
            case NonFatal(e)                 => Failure(AwaitFailure(AwaitFailReason.Unknown))
        }
    }
}

object Future {

    // TODO collect, join, firstOf (select), etc?

    //    def collect[A](fs: Seq[Future[A]]): Future[Seq[Result[A]]] = {
    //    }

    def apply[T](obj: => Result[T])(implicit executor: ExecutionContext): Future[T] = ConcreteFuture(ScalaFuture(obj))
    def success[T](obj: T): Future[T] = FutureSuccess(obj)
    def fail(failure: Failure.Detail): Future[Nothing] = FutureFail(failure)

    def immediateExecutor = new ExecutionContext {
        def reportFailure(t: Throwable) {}
        def execute(runnable: Runnable) { runnable.run() }
    }
}

case class ConcreteFuture[T](underlying: ScalaFuture[Result[T]]) extends Future[T]

case class FutureSuccess[T](obj: T) extends Future[T] {
    def underlying: ScalaFuture[Result[T]] = ScalaFuture.successful(Success(obj))
}

case class FutureFail(failure: Failure.Detail) extends Future[Nothing] {
    def underlying: ScalaFuture[Result[Nothing]] = ScalaFuture.successful(Failure(failure))
}