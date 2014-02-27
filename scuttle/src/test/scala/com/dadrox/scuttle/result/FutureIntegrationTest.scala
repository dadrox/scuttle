package com.dadrox.scuttle.result

import org.fictus.Fictus
import com.dadrox.scuttle.time._
import java.util.concurrent.atomic.AtomicInteger

class FutureIntegrationTest extends Fictus {
    implicit val ec = ExecutionContext.global

    @Test
    def join_success_waits_for_everything {
        val start = Time.now
        val count = new AtomicInteger(0)
        val fs = Vector(
            Future {
                Thread.sleep(100)
                count.incrementAndGet()
                Success(3)
            },
            Future {
                Thread.sleep(100)
                count.incrementAndGet()
                Success(4)
            },
            Future {
                Thread.sleep(100)
                count.incrementAndGet()
                Success(5)
            })

        Future.join(fs).await mustEqual Success(Void)

        val time = Time.now.since(start)
        val timeout = 150.ms
        if (time > timeout) fail(s"Futures must all finish within $timeout, but finished in $time")

        count.get mustEqual 3
    }
}