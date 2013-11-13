package com.dadrox.scuttle.time.joda

import com.dadrox.scuttle.time.{ Duration, Time }
import org.joda.time.{ DateTime => JodaTime, Duration => JodaDuration }

object converters {
    implicit class scuttleTimeToJodaTime(time: Time) {
        def asJoda(): JodaTime = conversions.scuttleTimeToJodaTime(time)
    }

    implicit class jodaTimeToScuttleTime(time: JodaTime) {
        def asScuttle(): Time = conversions.jodaTimeToScuttleTime(time)
    }

    implicit class scuttleDurationToJodaDuration(duration: Duration) {
        def asJoda(): JodaDuration = conversions.scuttleDurationToJodaDuration(duration)
    }

    implicit class jodaDurationToScuttleDuration(duration: JodaDuration) {
        def asScuttle(): Duration = conversions.jodaDurationToScuttleDuration(duration)
    }
}

object conversions {
    implicit def scuttleTimeToJodaTime(time: Time) = new JodaTime(time.milliseconds)
    implicit def jodaTimeToScuttleTime(time: JodaTime) = Time.fromMilliseconds(time.getMillis())

    implicit def scuttleDurationToJodaDuration(duration: Duration) = JodaDuration.millis(duration.inMilliseconds)
    implicit def jodaDurationToScuttleDuration(duration: JodaDuration) = Duration.fromMilliseconds(duration.getMillis())
}