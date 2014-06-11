package com.dadrox.scuttle.result

import org.fictus.Fictus
import com.dadrox.scuttle.time._
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.{ Await, Future => ScalaFuture, Promise, TimeoutException }
import java.util.concurrent.Executors
import com.dadrox.scuttle.concurrent.CountDownLatch
import com.dadrox.scuttle.concurrent.Executor

trait MakeSureItGotCalled {
    def success(): Unit
    def failure(): Unit
    def complete(): Unit
}

object FutureIntegrationTest extends Fictus {

    implicit val timer = Timer(threads = 1, daemonThreads = true)
    val executor = Executor.forkJoin()
    implicit val ec = ExecutionContext.fromExecutorService(executor)

    @AfterClass
    def stop {
        timer.stop()
        executor.shutdownNow()
    }
}

class FutureIntegrationTest extends Fictus {
    import FutureIntegrationTest._

    val makeSureItGotCalled = mock[MakeSureItGotCalled]

    @Test
    def join_success_waits_for_everything {
        val numberOfFutures = 4
        val sleepDuration = 100 ms

        val start = Time.now
        val count = new AtomicInteger(0)

        def newFuture(i: Int) = Future.async {
            Thread.sleep(sleepDuration.inMs)
            count.incrementAndGet()
            Success(i)
        }

        val fs = (1 to numberOfFutures).toVector.map(newFuture)
        Future.join(fs).await mustEqual Success(Void)

        val time = Time.now.since(start)
        val timeout = sleepDuration * numberOfFutures
        if (time > timeout) fail(s"Futures must all finish within $timeout, but finished in $time")

        count.get mustEqual numberOfFutures
    }

    @Test
    def within {
        val future = Future.async {
            Thread.sleep(5000)
            Success(3)
        }

        val start = Time.now
        val failThreshold = 300.ms
        future.within(100 ms).await() mustMatch {
            case Failure(FutureTimeout.Underlying, _, _) =>
                val done = Time.now.since(start)
                if (done > failThreshold) fail(s"Should have timed out in $failThreshold, but took $done")
        }
    }

    @Test
    def map {
        Future.async(Success(3)).map(_ + 2).map(_.toString()).await() mustEqual Success("5")
    }

    @Test
    def flatMap {
        val future = for {
            a <- Future.async(Success(3))
            b <- Future.async(Success(a + 2))
            c <- Future.async(Success(b.toString()))
        } yield c
        future.await() mustEqual Success("5")
    }

    @Test
    def collect {
        val future = Future.async(Success(3)) collect {
            case Success(it)                               => Success(it + 2)
            case failure @ Failure(reason, message, cause) => failure
        }
        future.await() mustEqual Success(5)
    }

    @Test
    def collectFlat {
        val future = Future.async(Success(3)) collectFlat {
            case Success(it)                               => Future.async(Success(it + 2))
            case failure @ Failure(reason, message, cause) => Future.fail(failure)
        }
        future.await() mustEqual Success(5)
    }

    @Test
    def flatten {
        Future.const(Future.const(3)).flatten.await() mustEqual Success(3)
    }

    @Test
    def failOnNone_success {
        val failure = Failure(Failure.NoReason, "Arrr")
        Some(3).failOnNone(failure) mustEqual Success(3)
    }

    @Test
    def failOnNone_failure {
        val failure = Failure(Failure.NoReason, "Arrr")
        None.failOnNone(failure) mustEqual failure
    }

    @Ignore // ugh so slooooowwww
    @Test
    def nested_awaits {
        val start = Time.now
        def time = Time.now.since(start)

        val result = Future.async {
            println(s">>>>>>>> evaluating outer $time")
            val result = Future.async {
                println(s">>>>>>>> evaluating 3 $time")
                Success(3)
            } //await ()

            println(s">>>>>>>> first result [$result] $time")

            result map {
                println(s">>>>>>>> adding 2 $time")
                _ + 2
            } await ()
        } await ()

        println(s">>>>>>>>> $result in $time")
    }

    @Ignore // intermittently fails :(
    @Test
    def future_throws {
        val latch = new CountDownLatch(2)

        val scalaFuture = ScalaFuture {
            throw new RuntimeException("EAT IT")
        }

        makeSureItGotCalled.failure
        makeSureItGotCalled.complete

        test {
            Future.apply { scalaFuture }
                .onFailure { it =>
                    makeSureItGotCalled.failure
                    latch.countDown()
                }(Future.immediateExecutor)
                .onComplete { it =>
                    makeSureItGotCalled.complete
                    latch.countDown()
                }(Future.immediateExecutor)
                .await()
            if (latch.await(2 seconds) == false) fail(s"Test timed out waiting for Future to complete")
        }
    }
}