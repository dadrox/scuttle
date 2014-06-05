package com.dadrox.scuttle.time

import scala.concurrent.duration.{ FiniteDuration => ScalaFiniteDuration }
import scala.concurrent.duration.{ Duration => ScalaDuration }
import com.dadrox.scuttle.time._

object conversions {
    implicit def javaDateToTime(date: java.util.Date): Time = date.asScuttle
    implicit def timeToJavaDate(time: Time): java.util.Date = time.asJava

    implicit def scalaDurationtoScuttle(duration: ScalaFiniteDuration): Duration = duration.asScuttle
    implicit def scuttleDurationToScala(duration: Duration): ScalaFiniteDuration = duration.asScala

    implicit def scalaInfToScuttle(duration: ScalaDuration.Infinite): Inf = duration.asScuttle
    implicit def scuttleInfinityToScala(duration: Inf): ScalaDuration = duration.asScala
}