package com.dadrox.scuttle.time

object Time extends TimeSource {
    private val defaultFormat = new TimeFormat("yyyyMMdd HH:mm:ss Z")
}

case class Time(milliseconds: Long) extends TimeInstance[Time] {
    import Time._

    override val ops = Time
    override def inMilliseconds() = milliseconds

    override lazy val toString = defaultFormat.format(this)
}

trait TimeSource extends DurationSource[Time] {
    def now() = fromMilliseconds(System.currentTimeMillis())

    val Epoch = new Time(0)

    override val Max = new Time(Long.MaxValue)
    override val Min = new Time(Long.MinValue)
    def fromMilliseconds(ms: Long) = new Time(ms)

    def fromDate(date: java.util.Date): Time = fromMilliseconds(date.getTime())

    def parse(pattern: String, date: String): Option[Time] = {
        try Some(new TimeFormat(pattern).parse(date))
        catch {
            case _ => None
        }
    }
}

trait TimeInstance[A <: TimeInstance[A]] extends DurationInstance[A] {
    def toDate(): java.util.Date = new java.util.Date(inMilliseconds)

    /** Formats the Time to UTC
     */
    def format(pattern: String): String = new TimeFormat(pattern).format(this)

    def since(then: A): Duration = this - then
    def until(then: A): Duration = then - this

    def -(then: TimeInstance[A]): Duration = Duration(inMilliseconds - then.inMilliseconds)

    // abs
}

class TimeFormat(pattern: String) {
    import java.util.TimeZone
    import java.text.SimpleDateFormat
    private val format = new SimpleDateFormat(pattern)
    format.setTimeZone(TimeZone.getTimeZone("UTC"))

    def format[A <: TimeInstance[A]](time: TimeInstance[A]): String = format.format(time.toDate)

    // TODO throws exceptions
    def parse(date: String): Time = Time.fromDate(format.parse(date))
}

/** Useful for time sensitive unit tests.
 *  Warning! This is _not_ thread safe! It is only intended for single-thread unit tests.
 */
object FakeTime extends FakeTimeInstance

class FakeTimeInstance extends TimeSource {
    import java.util.concurrent.atomic.AtomicLong

    // TODO ThreadLocal
    val current = new AtomicLong(System.currentTimeMillis())

    override def now() = Time.fromMilliseconds(current.get)

    def set(now: Time) { set(now.milliseconds) }
    def set(now: Long) { current.set(now) }

    def +(delta: Duration) = add(delta)
    def add(delta: Duration) = current.set(now.milliseconds + delta.milliseconds)
}