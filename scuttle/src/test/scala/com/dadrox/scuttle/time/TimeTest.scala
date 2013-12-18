package com.dadrox.scuttle.time

import org.fictus.Fictus
import java.util.Date

object TimeTest {
    // 20130520 @ exactly noon UTC
    val NowMs = 1369051200000L
    val Now = Time.fromMilliseconds(NowMs)
    val Epoch = Time.Epoch
}

class TimeTest extends DurationSourceTestBase(Time) with Fictus {
    import TimeTest._

    @Test
    def date_parsing {
        Time.parse("MM/dd/yyyy HH:mm:ss Z", "5/20/2013 12:00:00 -0600") mustEqual Some(Now + 6.hours)
        Time.parse("MM/dd/yyyy HH:mm:ss", "5/20/2013 12:00:00 -0600") mustEqual Some(Now)

        Time.parse("MM/dd/yyyy HH:mm:ss a", "5/20/2013 12:00:00 PM") mustEqual Some(Now)
        Time.parse("MM/dd/yyyy HH:mm:ss", "5/20/2013 12:00:00 PM") mustEqual Some(Now)
        Time.parse("MM/dd/yyyy", "5/20/2013 12:00:00 PM") mustEqual Some(Now - 12.hours)
        Time.parse("", "5/20/2013 12:00:00 PM") mustEqual None

        Time.parse("", "") mustEqual None
        Time.parse("MM/dd/yyyy", "") mustEqual None
    }

    @Test
    def at {
        Time.at(Now.toString) mustEqual Now
        Time at "20130520 12:00:00 -0000" mustEqual Now
        Time at "20130520 12:00:00 -0600" mustEqual Now + 6.hours
    }

    @Test
    def antitest_date_parsing_invalid_format_should_produce_None {
        Time.parse("MM", "5/20/2013 12:00:00 PM") mustEqual Some(Time.Epoch + 17.weeks + 1.day)
    }

    @Test
    def java_date_conversions {
        val date = new Date(0)

        Epoch.milliseconds mustEqual date.getTime()
        Epoch.toDate mustEqual date
        date mustEqual Epoch.toDate
        Time.fromDate(date) mustEqual Epoch
    }

    @Test
    def explicit_java_date_conversions {
        val epoch = Time.Epoch
        val date = new Date(0)

        date.asScuttle() mustEqual epoch
        epoch.asJava() mustEqual date
    }

    @Test
    def implicit_java_date_conversions {
        import com.dadrox.scuttle.time.conversions._

        val epoch = Time.Epoch
        val date = new Date(0)

        (date: Time) mustEqual epoch
        (epoch: Date) mustEqual date
    }

    @Test
    def adding {
        (Now + 3.seconds) mustEqual Time.fromMilliseconds(NowMs + 3 * 1000)
    }

    @Test
    def subtracting {
        (Now - 3.seconds) mustEqual Time.fromMilliseconds(NowMs - 3 * 1000)
    }

    @Test
    def floor {
        (Now + 30.minutes) floor 1.hour mustEqual Now
        (Now + 30.minutes) floor 12.hours mustEqual Now
        Now floor 12.hours mustEqual Now
        Now floor 5.hours mustEqual Now - 2.hours
    }

    @Test
    def ceiling {
        (Now + 30.minutes) ceiling 1.hour mustEqual Now + 1.hour
        (Now + 30.minutes) ceiling 12.hours mustEqual Now + 12.hours
        Now ceiling 12.hours mustEqual Now + 12.hours
        Now ceiling 5.hours mustEqual Now + 3.hours
    }

    @Test
    def midnight {
        Now.midnight0000 mustEqual Now - 12.hours
        Now.midnight2400 mustEqual Now + 12.hours
        (Now - 12.hours).midnight0000 mustEqual Now - 12.hours
        (Now - 12.hours).midnight2400 mustEqual Now + 12.hours

        (Now + 12.hours).midnight0000 mustEqual Now + 12.hours
        (Now + 12.hours).midnight2400 mustEqual Now + 36.hours
    }

    @Test
    def fake_time {
        val time = FakeTime().set(NowMs)
        val now = time.now
        now mustEqual Now
        (now + 3.seconds) mustEqual Time.fromMilliseconds(NowMs + 3 * 1000)

        time.set(0)
        Time.Epoch mustEqual time.now
    }

    @Test
    def since {
        Now since Epoch mustEqual Duration.fromMilliseconds(NowMs)
        Now - 10.minutes since Now - 1.day mustEqual 23.hours + 50.minutes
    }

    @Test
    def until {
        Epoch until Now mustEqual Duration.fromMilliseconds(NowMs)
        Now until Now + 1.day mustEqual 1.day
    }
}