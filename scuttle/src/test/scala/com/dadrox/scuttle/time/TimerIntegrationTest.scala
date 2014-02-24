package com.dadrox.scuttle.time

import com.dadrox.scuttle.result._
import com.dadrox.scuttle.time._
import com.dadrox.scuttle.time.conversions._
import org.fictus.Fictus
import scala.util.control.NonFatal

class TimerIntegrationTest extends Fictus {

    trait Service {
        def fired()
        def onSuccess()
        def onFailure()
        def onComplete()
    }

    implicit val ec = Future.immediateExecutor

    val svc = mock[Service]

    def withTimer(name: String, sleepFor: Duration)(fn: Timer => Unit) = test {
        println(s"$name $started")
        val timer = new PooledTimer(name = "TEST")
        try {
            fn(timer)
            Thread.sleep(sleepFor.inMilliseconds)
            println(s"$name DONE @ ${Time.now}")
        } finally timer.stop
    }

    @Before
    def before { println }

    def started = s"STARTED @ ${Time.now}"
    def fire = {
        println(s"FIRED @ ${Time.now}")
        svc.fired()
    }

    @Test
    def doAt {
        svc.fired()

        withTimer("doAt", 50 ms) {
            _.doAt(Time.now + 30.ms)(fire)
        }
    }

    @Test
    def doIn {
        svc.fired()

        withTimer("doIn", 50 ms) {
            _.doIn(30 ms)(fire)
        }
    }

    @Test
    def repeat_starting_now {
        svc.fired() times 2

        withTimer("repeat_starting_now", 100 ms) {
            _.repeat(40 ms)(fire)
        }
    }

    @Test
    def repeat {
        svc.fired() times 3

        withTimer("repeat", 100 ms) {
            _.repeat(Time.now + 50.ms, 20.ms)(fire)
        }
    }

    @Test
    def cancel {
        svc.fired() times 2

        withTimer("cancel", 50 ms) { unit =>
            val task = unit.repeat(20 ms)(fire)
            Thread.sleep(50)
            task.cancel()
        }
    }

    @Test
    def throws_with_task {
        withTimer("throws_with_task", 60 ms) { unit =>
            unit.repeat(Time.now + 50.ms, 20 ms)(throw new RuntimeException)
        }
    }

    @Test
    def throws_with_future {
        withTimer("throws_with_future", 60 ms) { unit =>
            val future = unit.doAt(Time.now + 50.ms)(throw new RuntimeException)
            println(future)
            future.onFailure(println)
            println(future.await(1 second))
        }
    }

    @Test
    def future_success_callbacks {
        svc.fired()
        svc.onComplete()
        svc.onSuccess()

        withTimer("future_success_callbacks", 60 ms) { unit =>
            val future = unit.doAt(Time.now + 50.ms) { fire; 3 }
            println(future)
            future.onSuccess(_ => svc.onSuccess())
                .onFailure(_ => svc.onFailure())
                .onComplete(_ => svc.onComplete())
                .await mustEqual Success(3)
        }
    }

    @Test
    def future_failure_callbacks {
        svc.onComplete()
        svc.onFailure()

        withTimer("future_success_callbacks", 60 ms) { unit =>
            val future = unit.doAt(Time.now + 50.ms) { throw new RuntimeException; 3 }
            println(future)
            future.onSuccess(_ => svc.onSuccess())
                .onFailure(_ => svc.onFailure())
                .onComplete(_ => svc.onComplete())
                .await mustMatch { case Failure(Timer.FailedScheduledTask, _, _) => }
        }
    }
}