package com.dadrox.scuttle

import scala.concurrent.duration.{ Duration => ScalaDuration }
import com.dadrox.scuttle.time.{ Duration, Time }

package object time {
    implicit def IntToDuration(value: Int) = new LongToDuration(value)

    implicit class LongToDuration(value: Long) {
        def milliseconds = Duration.fromMilliseconds(value)
        def millisecond = milliseconds
        def millis = milliseconds
        def milli = milliseconds
        def ms = milliseconds

        def seconds = Duration.fromSeconds(value)
        def second = seconds
        def sec = seconds
        def minutes = Duration.fromMinutes(value)
        def minute = minutes
        def min = minutes
        def hours = Duration.fromHours(value)
        def hour = hours
        def hr = hours
        def days = Duration.fromDays(value)
        def day = days
        def weeks = Duration.fromWeeks(value)
        def week = weeks
        def wk = weeks
    }

    implicit class JavaDateToScuttle(date: java.util.Date) {
        def asScuttle: Time = Time.fromDate(date)
    }

    implicit class ScuttleTimeToJava(time: Time) {
        def asJava: java.util.Date = time.toDate
    }

    implicit class ScalaDurationToScuttle(duration: ScalaDuration) {
        def asScuttle: Duration = Duration.fromMilliseconds(duration.toMillis)
    }

    implicit class ScuttleDurationToScala(duration: Duration) {
        def asScala: ScalaDuration = ScalaDuration(duration.inMilliseconds, scala.concurrent.duration.MILLISECONDS)
    }
}