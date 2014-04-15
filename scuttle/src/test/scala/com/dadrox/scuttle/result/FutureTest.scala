package com.dadrox.scuttle.result

import com.dadrox.scuttle.time._
import org.fictus.Fictus

sealed trait FailureDetail
case object FailMsg extends Failure.Reason with FailureDetail
case object AnotherFailMsg extends Failure.Reason with FailureDetail

class FutureTest extends Fictus {

    implicit val ec = Future.immediateExecutor
    implicit val timer = Timer()

    trait Service {
        def called(i: Int)
        def called(detail: FailureDetail)
        def called(result: Result[_])
    }
    val service = mock[Service]

    val failable = Failure(FailMsg, "fail")
    val anotherFailable = Failure(AnotherFailMsg, "failable")

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

        seven.await() mustEqual failable
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
        seven.await() mustEqual failable
    }

    @Test
    def flatten {
        Future.success(Future.success((3))).flatten.await mustEqual Success(3)
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
    def filter {
        (FutureSuccess(3) filter (3==)).await mustEqual Success(3)
        (FutureSuccess(3) filter (2==)).await mustMatch { case Failure(Failure.FilterPredicateFalse, _, _) => }

        val failedFuture: Future[Int] = Future.fail(failable)
        (failedFuture filter (3==)).await mustEqual failable
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
            case Failure(FutureTimeout.Await, _, _) =>
        }
    }

    @Test
    def apply {
        FutureSuccess(7).await(1.millisecond) mustEqual Success(7)
    }

    @Test
    def futureFail {
        FutureSuccess(3).flatMap(_ => FutureFail(failable)).await mustEqual failable
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

        val timeout = 1.millisecond
        val future = Future {
            Thread.sleep(100)
            Success(7)
        }
        future.within(timeout).await() mustMatch {
            case Failure(FutureTimeout.Underlying, _, _) =>
        }
    }

    @Test
    def within_noTimeout {
        import concurrent.ExecutionContext.Implicits.global
        val future = Future(Success(7))
        future.within(10.millisecond).await() mustEqual Success(7)
    }

    @Test
    def rescue_failureToSuccess {
        val result = 1
        Future.fail(failable).rescue {
            case failure => result
        }.await() mustEqual Success(result)
    }

    @Test
    def rescue_successToSuccess {
        val result = 1
        Future.success(result).rescue {
            case failure => 3
        }.await() mustEqual Success(result)
    }

    @Test
    def rescue_notDefinedAt {
        Future.fail(failable).rescue {
            case `anotherFailable` => "shouldMiss"
        }.await() mustEqual failable
    }

    @Test
    def rescueFlat_failureToFailure {
        Future.fail(failable).rescueFlat {
            case failure => Future.fail(anotherFailable)
        }.await() mustEqual anotherFailable
    }

    @Test
    def rescueFlat_failureToSuccess {
        val successValue = 3
        Future.fail(failable).rescueFlat {
            case failure => Future.success(successValue)
        }.await() mustEqual Success(successValue)
    }

    @Test
    def rescueFlat_successToSuccess {
        val successValue = 3
        Future.success(successValue).rescueFlat {
            case _ => Future.fail(anotherFailable)
        }.await() mustEqual Success(successValue)
    }

    @Test
    def rescueFlat_notDefinedAt {
        Future.fail(failable).rescueFlat {
            case `anotherFailable` => Future.fail(null)
        }.await() mustEqual failable
    }

    @Test
    def instance_collect_success {
        FutureSuccess(3).collect {
            case Success(it) => Success(it.toString)
            case f           => Success("0")
        }.await mustEqual Success("3")
    }

    @Test
    def instance_collect_failure {
        failable.collectFlat {
            case Success(it) => Success(it.toString)
            case f           => Success("0")
        }.await mustEqual Success("0")
    }

    @Test
    def instance_collectFlat_success {
        FutureSuccess(3).collectFlat {
            case Success(it) => FutureSuccess(it.toString)
            case f           => FutureSuccess("0")
        }.await mustEqual Success("3")
    }

    @Test
    def instance_collectFlat_failure {
        failable.collectFlat {
            case Success(it) => FutureSuccess(it.toString)
            case f           => FutureSuccess("0")
        }.await mustEqual Success("0")
    }

    @Test
    def object_collect_success {
        val fs = Vector(FutureSuccess(3), FutureSuccess(4))
        Future.collect(fs).await mustEqual Success(Seq(3, 4))
    }

    @Test
    def object_collect_fail {
        val fs: Seq[Future[Int]] = Vector(FutureSuccess(3), FutureFail(failable))
        Future.collect(fs).await mustEqual failable
    }

    //    @Test
    //    def collectAll_success {
    //        val fs = Vector(FutureSuccess(3), FutureSuccess(4))
    //        Future.collectAll(fs).await mustEqual Success(Seq(Success(3), Success(4)))
    //    }
    //
    //    @Test
    //    def collectAll_fail {
    //        val fs: Seq[Future[Int]] = Vector(FutureSuccess(3), FutureFail(failable))
    //        Future.collectAll(fs).await mustEqual Success(Seq(Success(3), failable))
    //    }

    @Test
    def join_success {
        val fs = Vector(FutureSuccess(3), FutureSuccess(4))
        Future.join(fs).await mustEqual Success(Void)
    }

    @Test
    def join_fail {
        val fs: Seq[Future[Int]] = Vector(FutureSuccess(3), FutureFail(failable))
        Future.join(fs).await mustEqual failable
    }

    @Test
    def failOnNone_fails_on_None {
        val future: Future[Option[Int]] = Future.success(None)
        val result = future failOnNone failable
        result.await mustEqual failable
    }

    @Test
    def failOnNone_succeeds_on_Some {
        val future: Future[Option[Int]] = Future.success(Some(3))
        val result = future failOnNone failable
        result.await mustEqual Success(3)
    }

    @Test
    def callInfo {
        Future.fail(failable).await().toString mustContain ("FutureTest")
        FutureFail(failable).await().toString mustContain ("FutureTest")
    }
}