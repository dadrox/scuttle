package com.dadrox.scuttle.time.joda

import org.fictus.Fictus
import com.dadrox.scuttle.time._
import org.joda.time.{ DateTime => JodaDateTime, Duration => JodaDuration }
import org.joda.time.{ LocalDate, LocalDateTime }
import org.joda.time.format.DateTimeFormat

class ConvertersTest extends Fictus {
    @Test
    def foo {
        def parse(pattern: String, date: String) = {
            val format = DateTimeFormat.forPattern(pattern)
            format.parseDateTime(date)
        }
        def jacked(pattern: String, date: String) = shouldThrow[IllegalArgumentException](println(parse(pattern, date)))

        jacked("MM", "5/20/2013 12:00:00 PM")
        jacked("MM", "5/20/2013 12:00:00 PM")

        parse("MM/dd/yyyy HH:mm:ss a", "5/20/2013 12:00:00 PM")
        jacked("MM/dd/yyyy HH:mm:ss", "5/20/2013 12:00:00 PM")
        jacked("MM/dd/yyyy", "5/20/2013 12:00:00 PM")
        jacked("", "5/20/2013 12:00:00 PM")

        parse("MM/dd/yyyy HH:mm:ss Z", "5/20/2013 12:00:00 -0600")
        jacked("MM/dd/yyyy HH:mm:ss", "5/20/2013 12:00:00 -0600")
    }

    @Test
    def jodaLocalDate_toScuttle {
        val eleven = new LocalDateTime(2014, 1, 1, 23, 0)

        eleven.toDateTime().asScuttle mustEqual Time.fromMilliseconds(1388642400000L)
    }

    @Test
    def jodaLocalDate_toScuttle_conversion {
        import com.dadrox.scuttle.time.joda.conversions.jodaTimeToScuttleTime
        val eleven = new LocalDateTime(2014, 1, 1, 23, 0)

        val time: Time = eleven.toDateTime()
        time mustEqual Time.fromMilliseconds(1388642400000L)
    }

    @Test
    def scuttleTimeToJoda_conversion {
        import com.dadrox.scuttle.time.joda.conversions.scuttleTimeToJodaTime

        val scuttle: JodaDateTime = Time.Epoch
        val joda = new JodaDateTime(0)

        scuttle mustEqual joda
        joda mustEqual scuttle
    }

    @Test
    def JodaDateTimeToScuttle_conversion {
        import com.dadrox.scuttle.time.joda.conversions.jodaTimeToScuttleTime

        val scuttle = Time.Epoch
        val joda: Time = new JodaDateTime(0)

        scuttle mustEqual joda
        joda mustEqual scuttle
    }

    @Test
    def scuttleDurationToJoda_conversion {
        import com.dadrox.scuttle.time.joda.conversions.scuttleDurationToJodaDuration

        val scuttle: JodaDuration = Duration.fromSeconds(3)
        val joda = JodaDuration.millis(3000)

        scuttle mustEqual joda
        joda mustEqual scuttle
    }

    @Test
    def jodaDurationToScuttle_conversion {
        import com.dadrox.scuttle.time.joda.conversions.jodaDurationToScuttleDuration

        val scuttle = Duration.fromSeconds(3)
        val joda: Duration = JodaDuration.millis(3000)

        scuttle mustEqual joda
        joda mustEqual scuttle
    }

    @Test
    def scuttleTimeToJoda_converter {
        val scuttle = Time.Epoch.asJoda
        val joda = new JodaDateTime(0)

        scuttle mustEqual joda
        joda mustEqual scuttle
    }

    @Test
    def jodaTimeToScuttle_converter {
        val scuttle = Time.Epoch
        val joda = new JodaDateTime(0).asScuttle

        scuttle mustEqual joda
        joda mustEqual scuttle
    }
    @Test
    def scuttleDurationToJoda_converter {
        val scuttle = Duration.fromSeconds(3).asJoda
        val joda = JodaDuration.millis(3000)

        scuttle mustEqual joda
        joda mustEqual scuttle
    }

    @Test
    def jodaDurationToScuttle_converter {
        val scuttle = Duration.fromSeconds(3)
        val joda = JodaDuration.millis(3000).asScuttle

        scuttle mustEqual joda
        joda mustEqual scuttle
    }
}