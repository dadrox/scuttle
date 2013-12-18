package com.dadrox.scuttle.time

import scala.util.control.NonFatal

object Time extends TimeSource

case class Time(milliseconds: Long) extends TimeInstance[Time] {
    import Time._

    override val ops = Time
    override def inMilliseconds() = milliseconds

    override lazy val toString = defaultFormat.format(this)
}

trait TimeSource extends DurationSource[Time] {
    def now() = fromMilliseconds(System.currentTimeMillis())

    val Epoch = new Time(0)
    val epoch = Epoch

    val Max = new Time(MaxMilliseconds)
    val Min = new Time(MinMilliseconds)
    def apply(ms: Long) = new Time(ms)

    def fromDate(date: java.util.Date): Time = fromMilliseconds(date.getTime())

    def parse(pattern: String, date: String): Option[Time] = {
        try Some(new TimeFormat(pattern).parse(date))
        catch {
            case NonFatal(_) => None
        }
    }

    private[time] val defaultFormat = new TimeFormat("yyyyMMdd HH:mm:ss Z")

    def at(date: String) = defaultFormat.parse(date)
}

trait TimeInstance[A <: TimeInstance[A]] extends DurationInstance[A] {
    def toDate(): java.util.Date = new java.util.Date(inMilliseconds)

    /** Formats the Time to UTC
     */
    def format(pattern: String): String = new TimeFormat(pattern).format(this)

    def since(when: A): Duration = this - when
    def until(when: A): Duration = when - this

    def -(when: TimeInstance[A]): Duration = Duration(inMilliseconds - when.inMilliseconds)

    def floor(d: Duration) = Time.fromMilliseconds(inMilliseconds / d.inMilliseconds * d.inMilliseconds)
    def ceiling(d: Duration) = floor(d) + d

    /** Midnight leading into the day. The beginning of the day. 00:00
     */
    def midnight0000() = floor(Duration.fromDays(1))

    /** Midnight at the end of the day. Techincally the beginning of the next day.
     */
    def midnight2400() = ceiling(Duration.fromDays(1))
}

object ljkakafsh extends App {
    val utc = java.util.TimeZone.getTimeZone("UTC")
    val tz = java.util.TimeZone.getTimeZone("America/Denver")
    //    val now = Time.now
    //    val xx = new java.util.SimpleTimeZone(tz.getOffset(now.inMillis), "don't care")

    def d(s: String) = {

        val dateFormat = new java.text.SimpleDateFormat("yyyyMMdd HH:mm:ss Z")
        dateFormat.setTimeZone(utc)
        println(s + ": " + dateFormat.format(Time.parse("yyyyMMddHHmm", s).get.toDate))
        dateFormat.setTimeZone(tz)
        println(s + ": " + dateFormat.format(Time.parse("yyyyMMddHHmm", s).get.toDate))

        val whenev = Time.fromSeconds(1371056400L)
        println(whenev + " - " + dateFormat.format(whenev.toDate))
    }

    d("201306121800")
}

class TimeFormat(pattern: String) {
    private def utcFormat = {
        val format = new java.text.SimpleDateFormat(pattern)
        format.setTimeZone(java.util.TimeZone.getTimeZone("UTC"))
        format
    }

    def format[A <: TimeInstance[A]](time: TimeInstance[A]): String = utcFormat.format(time.toDate)

    // TODO throws exceptions
    def parse(date: String): Time = Time.fromDate(utcFormat.parse(date))
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
    val current = new AtomicLong(System.currentTimeMillis())

    override def now() = Time.fromMilliseconds(current.get)

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
}