package com.dadrox.scuttle.result

import java.util.concurrent.atomic.{ AtomicReferenceArray, AtomicInteger }
import com.dadrox.scuttle.{ CallInfo, Enum }
import com.dadrox.scuttle.time.{ Time, Timer, Duration }
import scala.concurrent.{ Await, Future => ScalaFuture, Promise, TimeoutException, _ }
import scala.util.{ Success => ScalaSuccess, Failure => ScalaFailure }
import scala.util.control.NonFatal

sealed trait AwaitFailure extends Failure.Reason
object AwaitFailure {
    def apply(what: AwaitFailure, cause: Option[Throwable] = None) = Failure(what, s"Exception", cause)

    case object Interrupted extends AwaitFailure
    case object IllegalArgument extends AwaitFailure
    case object Unknown extends AwaitFailure
}

sealed trait FutureTimeout extends Failure.Timeout
object FutureTimeout {
    def apply(what: FutureTimeout, timeout: Option[Duration], cause: Option[Throwable] = None) = timeout match {
        case Some(timeout) => Failure(what, s"Timed out in $timeout", cause)
        case _             => Failure(what, s"Timed out", cause)
    }

    case object Await extends FutureTimeout
    case object Underlying extends FutureTimeout
}

trait Future[+T] {

    def underlying: ScalaFuture[Result[T]]

    def collect[U](handle: PartialFunction[Result[T], Result[U]])(implicit ec: ExecutionContext): Future[U] =
        ConcreteFuture(underlying.map { handle(_) })

    def collectFlat[U](handle: PartialFunction[Result[T], Future[U]])(implicit ec: ExecutionContext): Future[U] =
        ConcreteFuture(underlying.flatMap { handle(_).underlying })

    final def flatMap[U](f: T => Future[U])(implicit ec: ExecutionContext): Future[U] = ConcreteFuture {
        underlying.flatMap {
            case Success(obj)     => f(obj).underlying
            case failure: Failure => ScalaFuture.successful(failure)
        }
    }

    final def flatten[B](implicit ev: T <:< Future[B], ec: ExecutionContext): Future[B] = flatMap[B] { x => x }

    final def map[U](f: T => U)(implicit ec: ExecutionContext): Future[U] = ConcreteFuture(underlying.map(_.map(f)))

    final def foreach(fn: T => Unit)(implicit ec: ExecutionContext) = onSuccess(fn)

    final def filter(predicate: T => Boolean)(implicit ec: ExecutionContext): Future[T] = flatMap { r =>
        if (predicate(r)) FutureSuccess(r) else Future.failure(Failure.FilterPredicateFalse, s"filter predicated failed on $r")
    }

    final def withFilter(predicate: T => Boolean)(implicit ec: ExecutionContext): Future[T] = filter(predicate)

    final def onSuccess[U](fn: T => U)(implicit ec: ExecutionContext): Future[T] = {
        underlying.onSuccess {
            case Success(s) => fn(s)
            case _          =>
        }
        this
    }

    final def onComplete[U](fn: Result[T] => U)(implicit ec: ExecutionContext): Future[T] = {
        underlying.onComplete {
            case ScalaSuccess(s) => fn(s)
            case _               =>
        }
        this
    }

    final def onFailure[U](fn: Failure => U)(implicit ec: ExecutionContext): Future[T] = {
        underlying.onSuccess {
            case f: Failure => fn(f)
            case _          =>
        }
        this
    }

    final def rescue[U >: T](rescueFail: PartialFunction[Failure, U])(implicit ec: ExecutionContext): Future[U] = {
        ConcreteFuture(underlying.map {
            case f: Failure if (rescueFail.isDefinedAt(f)) => Success(rescueFail(f))
            case f: Failure                                => f
            case Success(s)                                => Success(s)
        })
    }

    final def rescueFlat[U >: T](rescueFail: PartialFunction[Failure, Future[U]])(implicit ec: ExecutionContext): Future[U] = {
        ConcreteFuture(underlying.flatMap {
            case f: Failure if (rescueFail.isDefinedAt(f)) => rescueFail(f).underlying
            case other                                     => ScalaFuture.successful(other)
        })
    }

    def within(timeout: Duration)(implicit timer: Timer, executor: ExecutionContext): Future[T] = {
        val p = Promise[Result[T]]()
        timer.doAt(Time.now + timeout)(p success FutureTimeout(FutureTimeout.Underlying, Some(timeout)))
        ConcreteFuture(ScalaFuture.firstCompletedOf(List(underlying, p.future)))
    }

    def await(): Result[T] = await(Duration.fromDays(1))

    def await(atMost: Duration): Result[T] = blocking {
        try Await.result(underlying, atMost.asScala)
        catch Future.handleThrowables()
    }
}

object Future {

    implicit class AugmentedFutureOption[A](future: Future[Option[A]])(implicit ec: ExecutionContext) {
        def failOnNone(failure: Failure): Future[A] = future flatMap {
            case Some(it) => Future.const(it)
            case None     => Future.fail(failure)
        }
    }

    // TODO firstOf (select), etc?
    // TODO add filter, which only keep successes and disposes of failures

    // TODO rename this to collectUntilFailure ?
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
                        case ScalaSuccess(fd: Failure) => if (!p.isCompleted) p.success(fd)
                        case ScalaFailure(f)           => if (!p.isCompleted) p.success(handleThrowables()(f))
                    }
                }

                ConcreteFuture(p.future)
        }
    }

    def join[A](fs: Seq[Future[A]])(implicit ec: ExecutionContext): Future[Void] = collect(fs) map (x => Void)

    /** Performs an asynchronous operation. */
    def async[T](result: => Result[T])(implicit ec: ExecutionContext): Future[T] = ConcreteFuture(ScalaFuture {
        try result catch {
            case NonFatal(e) => Failure(Failure.CaughtException, s"Underlying value threw $e", Some(e))
        }
    })

    /** Produces a constant Future for a Result that is already computed. */
    def value[T](result: => Result[T]): Future[T] = try {
        result match {
            case Success(value)   => const(value)
            case failure: Failure => Future.fail(failure)
        }
    } catch {
        case NonFatal(e) => Future.failure(Failure.CaughtException, s"Underlying value threw $e", Some(e))
    }

    /** Produces a constant Future for a value that is already computed. */
    def const[T](result: => T): Future[T] = try {
        FutureSuccess(result)
    } catch {
        case NonFatal(e) => Future.failure(Failure.CaughtException, s"Underlying const value threw $e", Some(e))
    }

    def apply[T](underlying: ScalaFuture[Result[T]])(implicit ec: ExecutionContext): Future[T] = ConcreteFuture(underlying.recover {
        case NonFatal(e) => Failure(Failure.CaughtException, s"Underlying Future threw $e", cause = Some(e))
    })

    def void: Future[Void] = const(Void)

    @deprecated("Use Future.value or Future.const", "20140611")
    def success[T](obj: T): Future[T] = FutureSuccess(obj)

    @deprecated("Use Future.fail or Future.failure", "20140611")
    def apply(failure: Failure)(implicit callInfo: CallInfo = CallInfo.callSite): Future[Nothing] = FutureFail(failure)

    def fail(failure: Failure)(implicit callInfo: CallInfo = CallInfo.callSite): Future[Nothing] = FutureFail(failure)
    def failure(reason: Failure.Reason, message: String, cause: Option[Throwable] = None)(implicit callInfo: CallInfo = CallInfo.callSite): Future[Nothing] = FutureFail(Failure(reason, message, cause))

    def immediateExecutor = new ExecutionContext {
        def reportFailure(t: Throwable) {}
        def execute(runnable: Runnable) { runnable.run() }
    }

    private def handleThrowables(timeout: Option[Duration] = None): PartialFunction[Throwable, Failure] = {
        case e: InterruptedException     => AwaitFailure(AwaitFailure.Interrupted, cause = Some(e))
        case e: TimeoutException         => FutureTimeout(FutureTimeout.Await, timeout, Some(e))
        case e: IllegalArgumentException => AwaitFailure(AwaitFailure.IllegalArgument, cause = Some(e))
        case NonFatal(e)                 => AwaitFailure(AwaitFailure.Unknown, cause = Some(e))
    }
}

private case class ConcreteFuture[T](underlying: ScalaFuture[Result[T]]) extends Future[T]

private case class FutureSuccess[T](obj: T) extends Future[T] {
    def underlying: ScalaFuture[Result[T]] = ScalaFuture.successful(Success(obj))
}

private case class FutureFail(failure: Failure)(
    implicit callInfo: CallInfo = CallInfo.callSite)
        extends Future[Nothing] {
    def underlying: ScalaFuture[Result[Nothing]] = ScalaFuture.successful(failure)
}