package com.dadrox.scuttle.twitter

import com.dadrox.scuttle.time.{ Duration, Time }
import com.twitter.util.{ Duration => TwitterDuration, Time => TwitterTime }

object converters {

}

object conversions {
    implicit def scuttleTimeToTwitterTime(time: Time) = TwitterTime.fromMilliseconds(time.milliseconds)
    implicit def twitterTimeToScuttleTime(time: TwitterTime) = Time.fromMilliseconds(time.inMilliseconds)

    implicit def scuttleDurationToTwitterDuration(duration: Duration) =
        TwitterDuration.fromTimeUnit(duration.inMilliseconds, java.util.concurrent.TimeUnit.MILLISECONDS)
    implicit def twitterDurationToScuttleDuration(duration: TwitterDuration) = Duration.fromMilliseconds(duration.inMilliseconds)
}