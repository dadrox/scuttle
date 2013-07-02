package com.dadrox.scuttle.time

object conversions {
    implicit def intToDuration(value: Int) = new AugmentedInteger(value)
    implicit def longToDuration(value: Long) = new AugmentedInteger(value)

    class AugmentedInteger(value: Long) {
        def milliseconds() = Duration.fromMilliseconds(value)
        def millisecond() = milliseconds
        def millis() = milliseconds
        def milli() = milliseconds
        def ms() = milliseconds

        def seconds() = Duration.fromSeconds(value)
        def second() = seconds
        def minutes() = Duration.fromMinutes(value)
        def minute() = minutes
        def hours() = Duration.fromHours(value)
        def hour() = hours
        def days() = Duration.fromDays(value)
        def day() = days
        def weeks() = Duration.fromWeeks(value)
        def week() = weeks
    }

    implicit def javaDateToTime(date: java.util.Date): Time = Time.fromDate(date)
    implicit def timeToJavaDate(time: Time): java.util.Date = time.toDate
}