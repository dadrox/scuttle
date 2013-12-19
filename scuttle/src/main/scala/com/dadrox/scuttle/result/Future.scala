package com.dadrox.scuttle.result

import java.util.concurrent.atomic.{ AtomicReferenceArray, AtomicInteger }
import com.dadrox.scuttle.{ CallInfo, Enum }
import com.dadrox.scuttle.time.{ Time, Timer, Duration }
import scala.concurrent.{ Await, Future => ScalaFuture, Promise, TimeoutException }
import scala.util.{ Success => ScalaSuccess, Failure => ScalaFailure }
import scala.util.control.NonFatal

object AwaitFailReason extends Enum {
    sealed case class EnumVal private[AwaitFailReason] (name: String) extends Value with Failure.Reason

    val Interrupted = EnumVal("Interrupted")
    val IllegalArgument = EnumVal("IllegalArgument")
    val Unknown = EnumVal("Unknown")
}
case class AwaitFailure(reason: AwaitFailReason.EnumVal, cause: Option[Throwable] = None) extends Failure.Detail {
    val message: String = reason.name
}

object TimeoutReason extends Enum {
    sealed case class EnumVal private[TimeoutReason] (name: String) extends Value with Failure.Reason

    val Await = EnumVal("Await")
    val Timer = EnumVal("Timer")
}
case class TimeoutFailure(reason: TimeoutReason.EnumVal, duration: Option[Duration], cause: Option[Throwable] = None) extends Failure.Detail {
    val message: String = reason.name
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

    final def foreach(fn: T => Unit)(implicit executor: ExecutionContext) = onSuccess(fn)

    final def filter(predicate: T => Boolean)(implicit executor: ExecutionContext): Future[T] = flatMap { r =>
        if (predicate(r)) FutureSuccess(r) else FutureFail(Failure.FilterPredicateFalse(r))
    }

    final def withFilter(predicate: T => Boolean)(implicit executor: ExecutionContext): Future[T] = filter(predicate)

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

    final def rescue[U >: T](rescueFail: PartialFunction[Failure, U])(implicit executor: ExecutionContext): Future[U] = {
        ConcreteFuture(underlying.map {
            case f: Failure if (rescueFail.isDefinedAt(f)) => Success(rescueFail(f))
            case f: Failure                                => f
            case Success(s)                                => Success(s)
        })
    }

    final def rescueFlat[U >: T](rescueFail: PartialFunction[Failure, Future[U]])(implicit executor: ExecutionContext): Future[U] = {
        ConcreteFuture(underlying.flatMap {
            case f: Failure if (rescueFail.isDefinedAt(f)) => rescueFail(f).underlying
            case other                                     => ScalaFuture.successful(other)
        })
    }

    def within(timeout: Duration)(implicit timer: Timer, executor: ExecutionContext): Future[T] = {
        val p = Promise[Result[T]]()
        timer.doAt(Time.now + timeout)(p success Failure(TimeoutFailure(TimeoutReason.Timer, Some(timeout))))
        ConcreteFuture(ScalaFuture.firstCompletedOf(List(underlying, p.future)))
    }

    def await(): Result[T] = await(Duration.fromDays(1))

    def await(atMost: Duration): Result[T] = {
        try Await.result(underlying, atMost.asScala)
        catch {
            case e: InterruptedException     => Failure(AwaitFailure(AwaitFailReason.Interrupted, cause = Some(e)))
            case e: TimeoutException         => Failure(TimeoutFailure(TimeoutReason.Await, Some(atMost), cause = Some(e)))
            case e: IllegalArgumentException => Failure(AwaitFailure(AwaitFailReason.IllegalArgument, cause = Some(e)))
            case NonFatal(e)                 => Failure(AwaitFailure(AwaitFailReason.Unknown, cause = Some(e)))
        }
    }
}

object Future {

    private def handleThrowables(timeout: Option[Duration] = None): PartialFunction[Throwable, Failure] = {
        case e: InterruptedException     => Failure(AwaitFailure(AwaitFailReason.Interrupted, cause = Some(e)))
        case e: TimeoutException         => Failure(TimeoutFailure(TimeoutReason.Await, timeout, cause = Some(e)))
        case e: IllegalArgumentException => Failure(AwaitFailure(AwaitFailReason.IllegalArgument, cause = Some(e)))
        case NonFatal(e)                 => Failure(AwaitFailure(AwaitFailReason.Unknown, cause = Some(e)))
    }

    // TODO collect, join, firstOf (select), etc?

    def collect[A](fs: Seq[Future[A]])(implicit ec: ExecutionContext): Future[Seq[A]] = {
        import scala.collection.mutable

        fs match {
            case Seq() => FutureSuccess(Seq())
            case results =>
                val p = Promise[Result[Seq[A]]]

                val results = new AtomicReferenceArray[A](fs.size)
                val count = new AtomicInteger(fs.size)
                for (i <- 0 until fs.size) {
                    val f = fs(i)
                    f.underlying.andThen {
                        case ScalaSuccess(Success(s)) =>
                            results.set(i, s)
                            if (count.decrementAndGet() <= 0) {
                                val resultsArray = new mutable.ArrayBuffer[A](fs.size)
                                for (j <- 0 until fs.size) resultsArray += results.get(j)
                                p.success(Success(resultsArray))
                            }
                        case ScalaSuccess(Failure(fd)) => if (!p.isCompleted) p.success(Failure(fd))
                        case ScalaFailure(f)           => if (!p.isCompleted) p.success(handleThrowables()(f))
                    }
                }

                ConcreteFuture(p.future)
        }
    }

    def apply[T](obj: => Result[T])(implicit executor: ExecutionContext): Future[T] = ConcreteFuture(ScalaFuture(obj))
    def apply[T](underlying: ScalaFuture[Result[T]]): Future[T] = ConcreteFuture(underlying)
    def success[T](obj: T): Future[T] = FutureSuccess(obj)
    def fail(failure: Failure.Detail)(implicit callInfo: CallInfo = CallInfo.callSite): Future[Nothing] = FutureFail(failure)

    def immediateExecutor = new ExecutionContext {
        def reportFailure(t: Throwable) {}
        def execute(runnable: Runnable) { runnable.run() }
    }
}

private case class ConcreteFuture[T](underlying: ScalaFuture[Result[T]]) extends Future[T]

private case class FutureSuccess[T](obj: T) extends Future[T] {
    def underlying: ScalaFuture[Result[T]] = ScalaFuture.successful(Success(obj))
}

private case class FutureFail(failure: Failure.Detail)(
    implicit callInfo: CallInfo = CallInfo.callSite)
        extends Future[Nothing] {
    def underlying: ScalaFuture[Result[Nothing]] = ScalaFuture.successful(Failure(failure))
}