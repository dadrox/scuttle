package com.dadrox.scuttle.joda

import com.dadrox.scuttle.time.{ Duration, Time }
import org.joda.time.{ DateTime => JodaTime, Duration => JodaDuration }

object converters {
    implicit def scuttleTimeToJodaTime(time: Time) = new {
        def asJoda(): JodaTime = conversions.scuttleTimeToJodaTime(time)
    }

    implicit def jodaTimeToScuttleTime(time: JodaTime) = new {
        def asScuttle(): Time = conversions.jodaTimeToScuttleTime(time)
    }

    implicit def scuttleDurationToJodaDuration(duration: Duration) = new {
        def asJoda(): JodaDuration = conversions.scuttleDurationToJodaDuration(duration)
    }

    implicit def jodaDurationToScuttleDuration(duration: JodaDuration) = new {
        def asScuttle(): Duration = conversions.jodaDurationToScuttleDuration(duration)
    }
}

object conversions {
    implicit def scuttleTimeToJodaTime(time: Time) = new JodaTime(time.milliseconds)
    implicit def jodaTimeToScuttleTime(time: JodaTime) = Time.fromMilliseconds(time.getMillis())

    implicit def scuttleDurationToJodaDuration(duration: Duration) = JodaDuration.millis(duration.inMilliseconds)
    implicit def jodaDurationToScuttleDuration(duration: JodaDuration) = Duration.fromMilliseconds(duration.getMillis())
}