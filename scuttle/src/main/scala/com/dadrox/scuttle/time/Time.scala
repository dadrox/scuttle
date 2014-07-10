package com.dadrox.scuttle.time

import scala.util.control.NonFatal

object Time extends TimeSource

case class Time(milliseconds: Long) extends TimeInstance[Time] {
    override val ops = Time
    override lazy val toString = Time.fullFormat.format(this)
    def toString(timeZone: String = "UTC", pattern: TimeFormat = Time.defaultFormat) = Time.defaultFormat.format(this, timeZone)
}

trait TimeSource extends DurationSource[Time] {
    def now = fromMilliseconds(System.currentTimeMillis)

    val Epoch = new Time(0)
    val epoch = Epoch

    val Max = new Time(MaxMilliseconds)
    val Min = new Time(MinMilliseconds)
    def apply(ms: Long) = new Time(ms)

    def fromDate(date: java.util.Date): Time = fromMilliseconds(date.getTime)

    def parse(pattern: String, date: String): Option[Time] = {
        try Some(new TimeFormat(pattern).parse(date))
        catch {
            case NonFatal(_) => None
        }
    }

    private[time] val fullFormat = new TimeFormat("yyyyMMdd HH:mm:ss.SSS Z")
    private[time] val defaultFormat = new TimeFormat("yyyyMMdd HH:mm:ss Z")

    def at(date: String) = defaultFormat.parse(date)
    def at(date: String, pattern: String) = new TimeFormat(pattern).parse(date)
}

trait TimeInstance[A <: TimeInstance[A]] extends DurationInstance[A] {
    def toDate: java.util.Date = new java.util.Date(milliseconds)

    /** Formats the Time to UTC */
    def format(pattern: String): String = new TimeFormat(pattern).format(this)
    def format(pattern: String, timeZone: String): String = new TimeFormat(pattern).format(this, timeZone)

    def since(when: A): Duration = this - when
    def until(when: A): Duration = when - this

    def -(when: TimeInstance[A]): Duration = Duration(milliseconds - when.milliseconds)

    def floor(d: Duration): Time = Time.fromMilliseconds(milliseconds / d.milliseconds * d.milliseconds)
    def ceiling(d: Duration): Time = if (milliseconds % d.milliseconds == 0) floor(d) else floor(d) + d

    /** Midnight leading into the day. The beginning of the day. 00:00 */
    def midnight0000 = floor(Duration.fromDays(1))

    /** Midnight at the end of the day. Techincally the beginning of the next day. */
    def midnight2400 = floor(Duration.fromDays(1)) + Duration.fromDays(1)
}

case class TimeFormat(pattern: String, tz: String = "UTC") {
    private def format(timeZone: String) = {
        val format = new java.text.SimpleDateFormat(pattern)
        format.setTimeZone(java.util.TimeZone.getTimeZone(timeZone))
        format
    }

    def format[A <: TimeInstance[A]](time: TimeInstance[A]): String = format(tz).format(time.toDate)
    def format[A <: TimeInstance[A]](time: TimeInstance[A], timeZone: String): String = format(timeZone).format(time.toDate)

    // TODO throws exceptions
    def parse(date: String): Time = Time.fromDate(format(tz).parse(date))
    def parse(date: String, tz: String): Time = Time.fromDate(format(tz).parse(date))

    def parseOption(date: String): Option[Time] = try Some(parse(date)) catch {
        case NonFatal(e) => None
    }
    def parseOption(date: String, tz: String): Option[Time] = try Some(parse(date, tz)) catch {
        case NonFatal(e) => None
    }
}

/** Useful for time sensitive unit tests.
 *  Warning! This is _not_ thread safe! It is only intended for single-thread unit tests.
 */
object FakeTime {
    def apply() = new FakeTimeInstance
}

class FakeTimeInstance extends TimeSource {
    import java.util.concurrent.atomic.AtomicLong

    // TODO ThreadLocal?
    val current = new AtomicLong(System.currentTimeMillis)

    override def now = Time.fromMilliseconds(current.get)

    def set(now: Time): FakeTimeInstance = {
        set(now.milliseconds)
        this
    }
    def set(now: Long): FakeTimeInstance = {
        current.set(now)
        this
    }

    def +(delta: Duration) = add(delta)
    def add(delta: Duration) = current.set(now.milliseconds + delta.milliseconds)
    override def toString = Time.fullFormat.format(now)
}