package com.dadrox.scuttle.time

import org.fictus.Fictus
import org.junit.Test
import java.util.Date
import com.dadrox.scuttle.time.converters.intToDuration

class TimeTest extends DurationOpsTestBase(Time) with Fictus {

    val Now = System.currentTimeMillis()
    val TimeNow = Time.fromMilliseconds(Now)

    @Test
    def java_date_conversions {
        val epoch = Time.Epoch
        val date = new Date(0)

        epoch.milliseconds mustEqual date.getTime()
        epoch.toDate mustEqual date
        date mustEqual epoch.toDate
        Time.fromDate(date) mustEqual epoch
    }

    @Test
    def implicit_java_date_conversions {
        import com.dadrox.scuttle.time.conversions.javaDateToTime
        import com.dadrox.scuttle.time.conversions.timeToJavaDate

        val epoch = Time.Epoch
        val date = new Date(0)

        (date: Time) mustEqual epoch
        (epoch: Date) mustEqual date
    }

    @Test
    def adding {
        (TimeNow + (3 seconds)) mustEqual Time.fromMilliseconds(Now + 3 * 1000)
    }

    @Test
    def subtracting {
        (TimeNow - (3 seconds)) mustEqual Time.fromMilliseconds(Now - 3 * 1000)
    }

    @Test
    def fake_time {
        FakeTime.set(Now)
        val now = FakeTime.now
        now mustEqual TimeNow
        (now + (3 seconds)) mustEqual Time.fromMilliseconds(Now + 3 * 1000)

        FakeTime.set(0)
        Time.Epoch mustEqual FakeTime.now
    }
}