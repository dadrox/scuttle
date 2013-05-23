package com.dadrox.scuttle.time

import org.fictus.Fictus
import org.junit.Test
import java.util.Date
import com.dadrox.scuttle.time.converters.intToDuration

object TimeFormatTest {
    // 20130520 @ exactly noon UTC
    val Now = 1369051200000L
    val TimeNow = Time.fromMilliseconds(Now)
}
class TimeFormatTest extends Fictus {

    FakeTime.set(TimeFormatTest.Now)
    val Now = FakeTime.now
    val TimeNow = TimeFormatTest.TimeNow

    @Test
    def format {
        new TimeFormat("yyyyMMdd").format(Now) mustEqual "20130520"
        new TimeFormat("yyyyMMdd HH:mm:ss.SSS").format(Now) mustEqual "20130520 12:00:00.000"
        new TimeFormat("yyyyMMdd HH:mm:ss.SSS Z").format(Now) mustEqual "20130520 12:00:00.000 +0000"
    }

    @Test
    def parsing {
        def parse(pattern: String, date: String) = Time.parse(pattern, date)

        parse("MM/dd/yyyy HH:mm:ss Z", "5/20/2013 12:00:00 -0600") mustEqual Some(TimeNow + 6.hours)
        parse("MM/dd/yyyy HH:mm:ss", "5/20/2013 12:00:00 -0600") mustEqual Some(TimeNow)

        parse("MM/dd/yyyy HH:mm:ss a", "5/20/2013 12:00:00 PM") mustEqual Some(TimeNow)
        parse("MM/dd/yyyy HH:mm:ss", "5/20/2013 12:00:00 PM") mustEqual Some(TimeNow)
        parse("MM/dd/yyyy", "5/20/2013 12:00:00 PM") mustEqual Some(TimeNow - 12.hours)
        parse("", "5/20/2013 12:00:00 PM") mustEqual None

        parse("", "") mustEqual None
        parse("MM/dd/yyyy", "") mustEqual None
    }

    @Test
    def antitest_date_parsing_invalid_format_should_produce_None {
        Time.parse("MM", "5/20/2013 12:00:00 PM") mustEqual Some(Time.Epoch + 17.weeks + 1.day)
    }
}
