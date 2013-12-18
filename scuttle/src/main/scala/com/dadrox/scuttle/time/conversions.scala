package com.dadrox.scuttle.time

import scala.concurrent.duration.{ Duration => ScalaDuration }

object conversions {
    implicit def javaDateToTime(date: java.util.Date): Time = Time.fromDate(date)
    implicit def timeToJavaDate(time: Time): java.util.Date = time.toDate

    implicit def scalaDurationtoScuttle(duration: ScalaDuration): Duration = Duration.fromMilliseconds(duration.toMillis)
    implicit def scuttleDurationToScala(duration: Duration): ScalaDuration = ScalaDuration(duration.inMilliseconds(), scala.concurrent.duration.MILLISECONDS)
}