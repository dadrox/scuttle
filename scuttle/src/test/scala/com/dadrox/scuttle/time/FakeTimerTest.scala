package com.dadrox.scuttle.time

import org.fictus.Fictus
import org.junit.Test
import com.dadrox.scuttle.result._

class FakeTimerTest extends Fictus {

    trait Service {
        def fired()
        def onSuccess()
        def onFailure()
        def onComplete()
    }

    implicit val ec = Future.immediateExecutor

    val svc = mock[Service]

    val time = FakeTime()
    val unit = Timer.fake(time)

    def fn = {
        svc.fired()

        // REMOVE
println("FIRED!")

        3
    }

    @Test
    def FakeTimer_one_shot_task {
        svc.fired()

        test {
            val future = unit.doAt(time.now + 30.minutes)(fn)
            time + 33.minutes
            unit.tick()

            future.await mustEqual Success(3)
        }
    }

    @Test
    def FakeTimer_one_shot_task_only_ever_invoked_once {
        svc.fired()

        test {
            val future = unit.doAt(time.now + 30.minutes)(fn)
            unit.tick()
            time + 33.minutes
            unit.tick()
            unit.tick()
            unit.tick()
        }
    }

    @Test
    def FakeTimer_periodic_task {
        svc.fired() times 2

        test {
            unit.repeat(30.minutes)(fn)
            unit.tick()
            time + 33.minutes

            println(time.now())

            unit.tick()
            unit.tick()
            time + 43.minutes
            unit.tick()
            unit.tick()
        }
    }
}