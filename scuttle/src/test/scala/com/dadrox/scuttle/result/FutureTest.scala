package com.dadrox.scuttle.result

import com.dadrox.scuttle.time._
import org.fictus.Fictus

case class FailMsg(message: String) extends Failure.Detail {
    val reason = new Failure.Reason {
        val name = "FailReason"
    }
    val cause: Option[Exception] = None
}
case class AnotherFailMsg(message: String) extends Failure.Detail {
    val reason = new Failure.Reason {
        val name = "FailReason"
    }
    val cause: Option[Exception] = None
}

class FutureTest extends Fictus {

    implicit val ec = Future.immediateExecutor
    implicit val timer = Timer()

    trait Service {
        def called(i: Int)
        def called(detail: Failure.Detail)
        def called(result: Result[_])
    }
    val service = mock[Service]

    val failable = FailMsg("fail")
    val anotherFailable = AnotherFailMsg("failable")

    @Test
    def forComprehension_success {
        val seven = for {
            three <- FutureSuccess(3)
            four <- FutureSuccess(4)
        } yield three + four
        seven.await() mustEqual Success(7)
    }

    @Test
    def forComprehension_failure {
        val seven: Future[Int] = for {
            three <- FutureSuccess(3)
            fail <- FutureFail(failable)
            four <- FutureSuccess(4)
        } yield three + four

        seven.await() mustEqual Failure(failable)
    }

    @Test
    def flatMaps {
        val seven = FutureSuccess(3) flatMap { s =>
            FutureSuccess(s + 4)
        } flatMap { s =>
            FutureSuccess(s + 5)
        } flatMap { _ =>
            FutureFail(failable)
        }
        seven.await() mustEqual Failure(failable)
    }

    @Test
    def foreach_success {
        val future: Future[Int] = FutureFail(failable)
        test(future foreach service.called)
    }

    @Test
    def foreach_failure {
        service.called(3)

        test(FutureSuccess(3) foreach service.called)
    }

    @Test
    def createFuture {
        FutureSuccess(7).await() mustEqual Success(7)
    }

    @Test
    def apply_timeout {
        import concurrent.ExecutionContext.Implicits.global

        val timeout = 1.millisecond
        Future {
            Thread.sleep(50)
            Success(7)
        }.await(timeout) mustMatch {
            case Failure(TimeoutFailure(_, Some(`timeout`), _)) =>
        }
    }

    @Test
    def apply {
        FutureSuccess(7).await(1.millisecond) mustEqual Success(7)
    }

    @Test
    def futureFail {
        FutureSuccess(3).flatMap(_ => FutureFail(failable)).await mustEqual Failure(failable)
    }

    @Test
    def onSuccess {
        service.called(3)
        test(FutureSuccess(3).onSuccess(service.called).await)
    }

    @Test
    def onComplete {
        service.called(Success(3))
        test(FutureSuccess(3).onComplete(service.called).await)
    }

    @Test
    def onFailure {
        service.called(failable)
        test(FutureFail(failable).onFailure(service.called).await)
    }

    @Test
    def within_timeout {
        import concurrent.ExecutionContext.Implicits.global

        val timeout = 1.millisecond()
        val future = Future {
            Thread.sleep(100)
            Success(7)
        }
        future.within(timeout).await() mustMatch {
            case Failure(TimeoutFailure(TimeoutReason.Timer, Some(`timeout`), _)) =>
        }
    }

    @Test
    def within_noTimeout {
        import concurrent.ExecutionContext.Implicits.global
        val future = Future(Success(7))
        future.within(10.millisecond()).await() mustEqual Success(7)
    }
}