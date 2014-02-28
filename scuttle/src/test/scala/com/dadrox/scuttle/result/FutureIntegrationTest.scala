package com.dadrox.scuttle.result

import org.fictus.Fictus
import com.dadrox.scuttle.time._
import java.util.concurrent.atomic.AtomicInteger

class FutureIntegrationTest extends Fictus {
    implicit val ec = ExecutionContext.global

    @Test
    def join_success_waits_for_everything {
        val numberOfFutures = 4
        val sleepDuration = 100 ms

        val start = Time.now
        val count = new AtomicInteger(0)

        def newFuture(i: Int) = Future {
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
}