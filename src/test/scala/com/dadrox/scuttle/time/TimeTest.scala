package com.dadrox.scuttle.time

import org.fictus.Fictus
import org.junit.Test
import java.util.Date
import com.dadrox.scuttle.time.converters.intToDuration

object TimeTest {
    val Now = System.currentTimeMillis()
    val TimeNow = Time.fromMilliseconds(Now)
    val Epoch = Time.Epoch
}
class TimeTest extends DurationOpsTestBase(Time) with Fictus {
    import TimeTest._

    @Test
    def java_date_conversions {
        val date = new Date(0)

        Epoch.milliseconds mustEqual date.getTime()
        Epoch.toDate mustEqual date
        date mustEqual Epoch.toDate
        Time.fromDate(date) mustEqual Epoch
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
        (TimeNow + 3.seconds) mustEqual Time.fromMilliseconds(Now + 3 * 1000)
    }

    @Test
    def subtracting {
        (TimeNow - 3.seconds) mustEqual Time.fromMilliseconds(Now - 3 * 1000)
    }

    @Test
    def fake_time {
        FakeTime.set(Now)
        val now = FakeTime.now
        now mustEqual TimeNow
        (now + 3.seconds) mustEqual Time.fromMilliseconds(Now + 3 * 1000)

        FakeTime.set(0)
        Time.Epoch mustEqual FakeTime.now
    }

    @Test
    def since {
        TimeNow since Epoch mustEqual Duration.fromMilliseconds(Now)
        TimeNow - 10.minutes since TimeNow - 1.day mustEqual 23.hours + 50.minutes
    }

    @Test
    def until {
        Epoch until TimeNow mustEqual Duration.fromMilliseconds(Now)
        TimeNow until TimeNow + 1.day mustEqual 1.day
    }
}