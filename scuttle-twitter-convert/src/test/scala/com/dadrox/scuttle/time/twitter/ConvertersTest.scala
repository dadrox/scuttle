package com.dadrox.scuttle.time.twitter

import org.fictus.Fictus
import com.dadrox.scuttle.time._
import com.twitter.util.{ Duration => TwitterDuration, Time => TwitterTime }

class ConvertersTest extends Fictus {
    @Test
    def scuttleTimeToTwitter_conversion {
        import com.dadrox.scuttle.time.twitter.conversions.scuttleTimeToTwitterTime

        val scuttle: TwitterTime = Time.Epoch
        val twitter = TwitterTime.epoch

        scuttle mustEqual twitter
        twitter mustEqual scuttle
    }

    @Test
    def twitterTimeToScuttle_conversion {
        import com.dadrox.scuttle.time.twitter.conversions.twitterTimeToScuttleTime

        val scuttle = Time.Epoch
        val twitter: Time = TwitterTime.epoch

        scuttle mustEqual twitter
        twitter mustEqual scuttle
    }
    @Test
    def scuttleDurationToTwitter_conversion {
        import com.dadrox.scuttle.time.twitter.conversions.scuttleDurationToTwitterDuration

        val scuttle: TwitterDuration = Duration.fromSeconds(3)
        val twitter = TwitterDuration.fromTimeUnit(3, java.util.concurrent.TimeUnit.SECONDS)

        scuttle mustEqual twitter
        twitter mustEqual scuttle
    }

    @Test
    def twitterDurationToScuttle_conversion {
        import com.dadrox.scuttle.time.twitter.conversions.twitterDurationToScuttleDuration

        val scuttle = Duration.fromSeconds(3)
        val twitter: Duration = TwitterDuration.fromTimeUnit(3, java.util.concurrent.TimeUnit.SECONDS)

        scuttle mustEqual twitter
        twitter mustEqual scuttle
    }

    @Test
    def scuttleTimeToTwitter_converter {
        import com.dadrox.scuttle.time.twitter.converters.scuttleTimeToTwitterTime

        val scuttle = Time.Epoch.asTwitter
        val twitter = TwitterTime.epoch

        scuttle mustEqual twitter
        twitter mustEqual scuttle
    }

    @Test
    def twitterTimeToScuttle_converter {
        import com.dadrox.scuttle.time.twitter.converters.twitterTimeToScuttleTime

        val scuttle = Time.Epoch
        val twitter = TwitterTime.epoch.asScuttle

        scuttle mustEqual twitter
        twitter mustEqual scuttle
    }
    @Test
    def scuttleDurationToTwitter_converter {
        import com.dadrox.scuttle.time.twitter.converters.scuttleDurationToTwitterDuration

        val scuttle = Duration.fromSeconds(3).asTwitter
        val twitter = TwitterDuration.fromTimeUnit(3, java.util.concurrent.TimeUnit.SECONDS)

        scuttle mustEqual twitter
        twitter mustEqual scuttle
    }

    @Test
    def twitterDurationToScuttle_converter {
        import com.dadrox.scuttle.time.twitter.converters.twitterDurationToScuttleDuration

        val scuttle = Duration.fromSeconds(3)
        val twitter = TwitterDuration.fromTimeUnit(3, java.util.concurrent.TimeUnit.SECONDS).asScuttle

        scuttle mustEqual twitter
        twitter mustEqual scuttle
    }
}