package com.dadrox.scuttle.time

import org.fictus.Fictus
import org.junit.Test
import java.util.Date

object TimeFormatTest {
    // 20130520 @ exactly noon
    val Now = 1369051200000L
}
class TimeFormatTest extends Fictus {

    FakeTime.set(TimeFormatTest.Now)
    val Now = FakeTime.now

    @Test
    def format {
        new TimeFormat("yyyyMMdd").format(Now) mustEqual "20130520"
        new TimeFormat("yyyyMMdd HH:mm:ss.SSS").format(Now) mustEqual "20130520 12:00:00.000"
        new TimeFormat("yyyyMMdd HH:mm:ss.SSS Z").format(Now) mustEqual "20130520 12:00:00.000 +0000"
    }
}

object Main extends App {
    println(System.currentTimeMillis)

    val d = new Date()
    d.setYear(2013)
    d.setMonth(5)
    d.setHours(12)
    d.setMinutes(0)
    d.setSeconds(0)
    println(d)
}