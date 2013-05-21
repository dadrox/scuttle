package com.dadrox.scuttle.time

import org.fictus.Fictus
import org.junit.Test

abstract class DurationSourceTestBase[A <: DurationInstance[A]](ops: DurationSource[A]) extends Fictus {

    @Test
    def simple_conversions {
        val ms = ops.fromMilliseconds(10000)
        ms.inWeeks mustEqual 0
        ms.inDays mustEqual 0
        ms.inHours mustEqual 0
        ms.inMinutes mustEqual 0
        ms.inSeconds mustEqual 10
        ms.inMillis mustEqual 10000L

        val s = ops.fromSeconds(5400)
        s.inWeeks mustEqual 0
        s.inDays mustEqual 0
        s.inHours mustEqual 1
        s.inMinutes mustEqual 90
        s.inSeconds mustEqual 5400
        s.inMillis mustEqual 5400 * 1000L

        val m = ops.fromMinutes(2)
        m.inWeeks mustEqual 0
        m.inDays mustEqual 0
        m.inHours mustEqual 0
        m.inMinutes mustEqual 2
        m.inSeconds mustEqual 2 * 60
        m.inMillis mustEqual 2 * 60 * 1000L

        val h = ops.fromHours(3)
        h.inWeeks mustEqual 0
        h.inDays mustEqual 0
        h.inHours mustEqual 3
        h.inMinutes mustEqual 3 * 60
        h.inSeconds mustEqual 3 * 60 * 60
        h.inMilliseconds mustEqual 3 * 60 * 60 * 1000L

        val d = ops.fromDays(365)
        d.inWeeks mustEqual 52
        d.inDays mustEqual 365
        d.inHours mustEqual 365 * 24
        d.inMinutes mustEqual 365 * 24 * 60
        d.inSeconds mustEqual 365 * 24 * 60 * 60
        d.inMilliseconds mustEqual 365 * 24 * 60 * 60 * 1000L

        val w = ops.fromWeeks(5)
        w.inWeeks mustEqual 5
        w.inDays mustEqual 5 * 7
        w.inHours mustEqual 5 * 7 * 24
        w.inMinutes mustEqual 5 * 7 * 24 * 60
        w.inSeconds mustEqual 5 * 7 * 24 * 60 * 60
        w.inMilliseconds mustEqual 5 * 7 * 24 * 60 * 60 * 1000L
    }
}