package com.dadrox.scuttle.time

class DurationTest extends DurationSourceTestBase(Duration) with org.fictus.Fictus {
    @Test
    def int_implicits {
        3.seconds mustEqual Duration.fromSeconds(3)
    }

    @Test
    def long_implicits {
        3L.seconds mustEqual Duration.fromSeconds(3)
    }

    @Test
    def implicit_scala_duration_conversions {
        import com.dadrox.scuttle.time.conversions._
        import scala.concurrent.duration.FiniteDuration
        import scala.concurrent.duration.{ Duration => ScalaDuration }

        val scalaFiniteDuration = FiniteDuration(1, scala.concurrent.duration.MILLISECONDS)
        val scalaDuration = ScalaDuration(1, scala.concurrent.duration.MILLISECONDS)
        val scuttleDuration = 1.ms

        (scalaFiniteDuration: Duration) mustEqual scuttleDuration
        (scalaDuration: Duration) mustEqual scuttleDuration
        (scuttleDuration: scala.concurrent.duration.Duration) mustEqual scalaDuration

        shouldThrow[IllegalArgumentException] {
            (ScalaDuration.Inf: Duration) mustEqual Duration.Infinite
        }
    }

    @Test
    def explicit_scala_duration_conversions {
        import com.dadrox.scuttle.time._
        import scala.concurrent.duration.FiniteDuration
        import scala.concurrent.duration.{ Duration => ScalaDuration }

        val scalaFiniteDuration = FiniteDuration(1, scala.concurrent.duration.MILLISECONDS)
        val scalaDuration = ScalaDuration(1, scala.concurrent.duration.MILLISECONDS)
        val scuttleDuration = 1.ms

        scalaFiniteDuration.asScuttle mustEqual scuttleDuration
        scalaDuration.asScuttle mustEqual scuttleDuration
        scuttleDuration.asScala mustEqual scalaDuration

        shouldThrow[IllegalArgumentException] {
            ScalaDuration.Inf.asScuttle mustEqual Duration.Infinite
        }
    }

    @Test
    def unary_- {
        -3.seconds mustEqual Duration.fromSeconds(-3)
    }

    @Test
    def abs {
        -3.seconds.abs mustEqual 3.seconds
        -0.seconds.abs mustEqual 0.seconds
    }

    @Test
    def adding {
        3.seconds + 3.seconds mustEqual 6.seconds
    }

    @Test
    def subtracting {
        6.seconds - 3.seconds mustEqual 3.seconds
    }

    @Test
    def scalar_multiply {
        3.seconds * 2 mustEqual 6.seconds
        3.seconds * 2L mustEqual 6.seconds
    }

    @Test
    def scalar_division {
        6.seconds / 2 mustEqual 3.seconds
        6.seconds / 2L mustEqual 3.seconds
    }

    @Test
    def toStrings_are_decent {
        -3.milliseconds.toString mustEqual "-3.milliseconds"
        3.milliseconds.toString mustEqual "+3.milliseconds"
        3.seconds.toString mustEqual "+3.seconds"
        3.minutes.toString mustEqual "+3.minutes"
        3.hours.toString mustEqual "+3.hours"
        3.days.toString mustEqual "+3.days"
        3.weeks.toString mustEqual "+3.weeks"

        (1.week + 1.day + 1.hour + 1.minute + 1.second + 1.ms).toString
            .mustEqual("+1.week+1.day+1.hour+1.minute+1.second+1.millisecond")

        (-(1.week + 1.day + 1.hour + 1.minute + 1.second + 1.ms)).toString
            .mustEqual("-1.week-1.day-1.hour-1.minute-1.second-1.millisecond")

        (1.week + 1.day + 1.hour + 1.minute + 1.second + 1.ms).toString(terse = true)
            .mustEqual("+1.w+1.d+1.h+1.m+1.s+1.ms")

        Duration.NegativeInfinite.toString mustEqual NegativeInfinity.toString
        Duration.Infinite.toString mustEqual Infinity.toString
        (Duration.fromMilliseconds(Long.MinValue + 2)).toString mustEqual "-15250284452.weeks-3.days-7.hours-12.minutes-55.seconds-806.milliseconds"
        (Duration.fromMilliseconds(Long.MaxValue - 1)).toString mustEqual "+15250284452.weeks+3.days+7.hours+12.minutes+55.seconds+806.milliseconds"
        Duration.Infinite.toString(TimeUnit.Millisecond) mustEqual "+9223372036854775807.milliseconds"

        1000.ms.toString mustEqual "+1.second"
        60.seconds.toString mustEqual "+1.minute"
        60.minutes.toString mustEqual "+1.hour"
        24.hours.toString mustEqual "+1.day"
        7.days.toString mustEqual "+1.week"

        2000.ms.toString mustEqual "+2.seconds"
        120.seconds.toString mustEqual "+2.minutes"
        120.minutes.toString mustEqual "+2.hours"
        48.hours.toString mustEqual "+2.days"
        14.days.toString mustEqual "+2.weeks"
    }

    @Test
    def infinite_duration {
        Infinity.inMilliseconds mustEqual Duration.Infinite.inMilliseconds
        Infinity + 3.weeks mustEqual Infinity
        Infinity - 3.weeks mustEqual Infinity
        Infinity * 3 mustEqual Infinity
        Infinity / 3 mustEqual Infinity
        Infinity % 3 mustEqual Infinity
        Infinity.abs mustEqual Infinity
        -Infinity mustEqual NegativeInfinity
        Infinity.finite_?() mustEqual false
    }

    @Test
    def negative_infinite_duration {
        NegativeInfinity.inMilliseconds mustEqual Duration.NegativeInfinite.inMilliseconds
        NegativeInfinity + 3.weeks mustEqual NegativeInfinity
        NegativeInfinity - 3.weeks mustEqual NegativeInfinity
        NegativeInfinity * 3 mustEqual NegativeInfinity
        NegativeInfinity / 3 mustEqual NegativeInfinity
        NegativeInfinity % 3 mustEqual NegativeInfinity
        NegativeInfinity.abs mustEqual Infinity
        -NegativeInfinity mustEqual Infinity
        NegativeInfinity.finite_?() mustEqual false
    }

    @Test
    def divide_by_zero {
        shouldThrow[java.lang.ArithmeticException](3.seconds / 0)
    }

    @Test
    def duration_overflows_to_inifinity {
        Duration.Infinite + 1.ms mustEqual Infinity
        Duration.Infinite - -1.ms mustEqual Infinity
        Duration.Infinite * 2 mustEqual Infinity
        Duration.NegativeInfinite * -2 mustEqual Infinity

        (Duration.fromMilliseconds(Long.MaxValue - 1) / 2 + 1.ms) * 2 mustEqual Infinity

        Long.MaxValue.milliseconds mustEqual Infinity
    }

    @Test
    def duration_underflows_to_negative_inifinity {
        Duration.NegativeInfinite + -1.ms mustEqual NegativeInfinity
        Duration.NegativeInfinite - 1.ms mustEqual NegativeInfinity
        Duration.NegativeInfinite * 2 mustEqual NegativeInfinity
        Duration.Infinite * -2 mustEqual NegativeInfinity
        (Long.MinValue + 1).milliseconds mustEqual NegativeInfinity
        Long.MinValue.milliseconds mustEqual NegativeInfinity
    }
}