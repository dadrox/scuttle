package com.dadrox.scuttle.time

import com.twitter.util.{ Duration => TwitterDuration, Time => TwitterTime }

package object twitter {
    implicit class scuttleTimeToTwitterTime(time: Time) {
        def asTwitter: TwitterTime = conversions.scuttleTimeToTwitterTime(time)
    }

    implicit class twitterTimeToScuttleTime(time: TwitterTime) {
        def asScuttle: Time = conversions.twitterTimeToScuttleTime(time)
    }

    implicit class scuttleDurationToTwitterDuration(duration: Duration) {
        def asTwitter: TwitterDuration = conversions.scuttleDurationToTwitterDuration(duration)
    }

    implicit class twitterDurationToScuttleDuration(duration: TwitterDuration) {
        def asScuttle: Duration = conversions.twitterDurationToScuttleDuration(duration)
    }

    object conversions {
        implicit def scuttleTimeToTwitterTime(time: Time) = TwitterTime.fromMilliseconds(time.milliseconds)
        implicit def twitterTimeToScuttleTime(time: TwitterTime) = Time.fromMilliseconds(time.inMilliseconds)

        implicit def scuttleDurationToTwitterDuration(duration: Duration) =
            TwitterDuration.fromTimeUnit(duration.inMilliseconds, java.util.concurrent.TimeUnit.MILLISECONDS)
        implicit def twitterDurationToScuttleDuration(duration: TwitterDuration) = Duration.fromMilliseconds(duration.inMilliseconds)
    }
}