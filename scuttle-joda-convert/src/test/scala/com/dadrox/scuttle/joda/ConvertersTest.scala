package com.dadrox.scuttle.joda

import org.fictus.Fictus
import org.junit.Test
import com.dadrox.scuttle.time.{ Duration, Time }
import org.joda.time.{ DateTime => JodaTime, Duration => JodaDuration }

class ConvertersTest extends Fictus {
    @Test
    def scuttleTimeToJoda_conversion {
        import com.dadrox.scuttle.joda.conversions.scuttleTimeToJodaTime

        val scuttle: JodaTime = Time.Epoch
        val joda = new JodaTime(0)

        scuttle mustEqual joda
        joda mustEqual scuttle
    }

    @Test
    def jodaTimeToScuttle_conversion {
        import com.dadrox.scuttle.joda.conversions.jodaTimeToScuttleTime

        val scuttle = Time.Epoch
        val joda: Time = new JodaTime(0)

        scuttle mustEqual joda
        joda mustEqual scuttle
    }
    @Test
    def scuttleDurationToJoda_conversion {
        import com.dadrox.scuttle.joda.conversions.scuttleDurationToJodaDuration

        val scuttle: JodaDuration = Duration.fromSeconds(3)
        val joda = JodaDuration.millis(3000)

        scuttle mustEqual joda
        joda mustEqual scuttle
    }

    @Test
    def jodaDurationToScuttle_conversion {
        import com.dadrox.scuttle.joda.conversions.jodaDurationToScuttleDuration

        val scuttle = Duration.fromSeconds(3)
        val joda: Duration = JodaDuration.millis(3000)

        scuttle mustEqual joda
        joda mustEqual scuttle
    }

    @Test
    def scuttleTimeToJoda_converter {
        import com.dadrox.scuttle.joda.converters.scuttleTimeToJodaTime

        val scuttle = Time.Epoch.asJoda
        val joda = new JodaTime(0)

        scuttle mustEqual joda
        joda mustEqual scuttle
    }

    @Test
    def jodaTimeToScuttle_converter {
        import com.dadrox.scuttle.joda.converters.jodaTimeToScuttleTime

        val scuttle = Time.Epoch
        val joda = new JodaTime(0).asScuttle

        scuttle mustEqual joda
        joda mustEqual scuttle
    }
    @Test
    def scuttleDurationToJoda_converter {
        import com.dadrox.scuttle.joda.converters.scuttleDurationToJodaDuration

        val scuttle = Duration.fromSeconds(3).asJoda
        val joda = JodaDuration.millis(3000)

        scuttle mustEqual joda
        joda mustEqual scuttle
    }

    @Test
    def jodaDurationToScuttle_converter {
        import com.dadrox.scuttle.joda.converters.jodaDurationToScuttleDuration

        val scuttle = Duration.fromSeconds(3)
        val joda = JodaDuration.millis(3000).asScuttle

        scuttle mustEqual joda
        joda mustEqual scuttle
    }
}