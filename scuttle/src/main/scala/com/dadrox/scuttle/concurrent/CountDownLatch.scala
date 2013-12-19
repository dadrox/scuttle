package com.dadrox.scuttle.concurrent

import com.dadrox.scuttle.time._
import java.util.concurrent.{ TimeUnit => JTimeUnit }

class CountDownLatch(initialCount: Int) {
    private val latch = new java.util.concurrent.CountDownLatch(initialCount)

    def await(): Unit = latch.await()
    def await(duration: Duration): Boolean = latch.await(duration.inMilliseconds, JTimeUnit.MILLISECONDS)
    def countDown() = latch.countDown()
    def count(): Long = latch.getCount()
}