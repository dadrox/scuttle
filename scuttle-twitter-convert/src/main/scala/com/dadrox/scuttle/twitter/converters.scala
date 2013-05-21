package com.dadrox.scuttle.twitter

import com.dadrox.scuttle.time.{ Duration, Time }
import com.twitter.util.{ Duration => TwitterDuration, Time => TwitterTime }

object converters {
    implicit def scuttleTimeToTwitterTime(time: Time) = new {
        def asTwitter(): TwitterTime = conversions.scuttleTimeToTwitterTime(time)
    }

    implicit def twitterTimeToScuttleTime(time: TwitterTime) = new {
        def asScuttle(): Time = conversions.twitterTimeToScuttleTime(time)
    }

    implicit def scuttleDurationToTwitterDuration(duration: Duration) = new {
        def asTwitter(): TwitterDuration = conversions.scuttleDurationToTwitterDuration(duration)
    }

    implicit def twitterDurationToScuttleDuration(duration: TwitterDuration) = new {
        def asScuttle(): Duration = conversions.twitterDurationToScuttleDuration(duration)
    }
}

object conversions {
    implicit def scuttleTimeToTwitterTime(time: Time) = TwitterTime.fromMilliseconds(time.milliseconds)
    implicit def twitterTimeToScuttleTime(time: TwitterTime) = Time.fromMilliseconds(time.inMilliseconds)

    implicit def scuttleDurationToTwitterDuration(duration: Duration) =
        TwitterDuration.fromTimeUnit(duration.inMilliseconds, java.util.concurrent.TimeUnit.MILLISECONDS)
    implicit def twitterDurationToScuttleDuration(duration: TwitterDuration) = Duration.fromMilliseconds(duration.inMilliseconds)
}