package com.dadrox.scuttle.time

import org.fictus.Fictus
import org.junit.Test
import scala.util.control.NonFatal
import org.junit.Before
import com.dadrox.scuttle.result._
import com.dadrox.scuttle.time.conversions._

class TimerIntegrationTest extends Fictus {
    implicit val ec = Future.immediateExecutor

    def withTimer(sleepFor: Duration)(fn: Timer => Unit) = {
        val timer = new PooledTimer("TEST")
        try {
            fn(timer)
            Thread.sleep(sleepFor.inMilliseconds())
            println(s"DONE @ ${Time.now}")
        } finally timer.stop
    }

    @Before
    def before { println }

    def started = s"STARTED @ ${Time.now}"
    def fire = println(s"FIRED @ ${Time.now}")

    @Test
    def doAt {
        println(s"doAt $started")
        withTimer(2 seconds) {
            _.doAt(Time.now + 1.second)(fire)
        }
    }

    @Test
    def doIn {
        println(s"doIn $started")
        withTimer(2 seconds) {
            _.doIn(1.second)(fire)
        }
    }

    @Test
    def repeat_starting_now {
        println(s"repeat_starting_now $started")
        withTimer(6 seconds) {
            _.repeat(1.second)(fire)
        }
    }

    @Test
    def repeat {
        println(s"repeat $started")
        withTimer(6 seconds) {
            _.repeat(Time.now + 1.second, 1.second)(fire)
        }
    }

    @Test
    def cancel {
        println(s"cancel STARTED @ ${Time.now}")
        withTimer(1 seconds) { unit =>
            val task = unit.repeat(Time.now + 1.second, 1.second)(fire)
            task.cancel()
        }
    }

    @Test
    def throws_with_task {
        println(s"throws_with_task STARTED @ ${Time.now}")
        withTimer(2 seconds) { unit =>
            unit.repeat(Time.now + 1.second, 1.second)(throw new RuntimeException)
        }
    }

    @Test
    def throws_with_future {
        println(s"throws_with_future STARTED @ ${Time.now}")
        withTimer(2 seconds) { unit =>
            val future = unit.doAt(Time.now + 1.second)(throw new RuntimeException)
            println(future)
            println(future.await(2 seconds))
        }
    }

    @Test
    def future {
        println(s"future $started")
        withTimer(2.seconds) { unit =>
            val future = unit.doAt(Time.now + 1.second) { fire; 3 }
            println(future)
            future.onSuccess(_ => println("SUCCESS"))
                .onFailure(_ => println("FAILURE"))
                .onComplete(_ => println("COMPLETE"))
                .await mustEqual Success(3)
        }
    }
}