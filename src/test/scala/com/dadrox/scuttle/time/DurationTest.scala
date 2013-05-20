package com.dadrox.scuttle.time

import org.junit.Test
import org.fictus.Fictus
import com.dadrox.scuttle.time.converters.intToDuration

class DurationTest extends DurationOpsTestBase(Duration) with Fictus {
    @Test
    def int_implicits {
        import com.dadrox.scuttle.time.converters.intToDuration

        (3 seconds) mustEqual Duration.fromSeconds(3)
    }

    @Test
    def long_implicits {
        import com.dadrox.scuttle.time.converters.longToDuration

        (3L seconds) mustEqual Duration.fromSeconds(3)
    }

    @Test
    def adding {
        ((3 seconds) + (3 seconds)) mustEqual (6 seconds)
    }

    @Test
    def subtracting {
        ((6 seconds) - (3 seconds)) mustEqual (3 seconds)
    }

    @Test
    def scalar_multiply {
        (3 seconds) * 2 mustEqual (6 seconds)
        (3 seconds) * 2L mustEqual (6 seconds)
    }

    @Test
    def scalar_division {
        (6 seconds) / 2 mustEqual (3 seconds)
        (6 seconds) / 2L mustEqual (3 seconds)
    }

    @Test
    def toStrings_are_decent {
        (3 milliseconds).toString mustEqual "+3.milliseconds"
        (3 seconds).toString mustEqual "+3.seconds"
        (3 minutes).toString mustEqual "+3.minutes"
        (3 hours).toString mustEqual "+3.hours"
        (3 days).toString mustEqual "+3.days"
        (3 weeks).toString mustEqual "+3.weeks"

        ((1 week) + (1 day) + (1 hour) + (1 minute) + (1 second) + (1 ms)).toString
            .mustEqual("+1.week+1.day+1.hour+1.minute+1.second+1.millisecond")

        (1000 ms).toString mustEqual "+1.second"
        (60 seconds).toString mustEqual "+1.minute"
        (60 minutes).toString mustEqual "+1.hour"
        (24 hours).toString mustEqual "+1.day"
        (7 days).toString mustEqual "+1.week"

        (2000 ms).toString mustEqual "+2.seconds"
        (120 seconds).toString mustEqual "+2.minutes"
        (120 minutes).toString mustEqual "+2.hours"
        (48 hours).toString mustEqual "+2.days"
        (14 days).toString mustEqual "+2.weeks"
    }
}