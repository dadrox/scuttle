package com.dadrox.scuttle.time

object Time extends TimeSource {
    private val defaultFormat = new TimeFormat("yyyyMMdd HH:mm:ss Z")
}

case class Time(milliseconds: Long) extends TimeLikeOps[Time] {
    import Time._

    override val ops = Time
    override def inMilliseconds() = milliseconds

    /** Formats the Time to UTC
     */
    def format(pattern: String): String = new TimeFormat(pattern).format(this)

    def since(then: Time): Duration = this - then
    def until(then: Time): Duration = then - this

    override lazy val toString = defaultFormat.format(this)
}

trait TimeLike[A <: TimeLikeOps[A]] extends DurationLike[A] {
    def fromDate(date: java.util.Date): Time = Time.fromMilliseconds(date.getTime())
}

trait TimeLikeOps[A <: TimeLikeOps[A]] extends DurationLikeOps[A] {
    def toDate(): java.util.Date = new java.util.Date(inMilliseconds)

    def -(then: Time): Duration = Duration(inMilliseconds - then.inMilliseconds)
}

trait TimeSource extends TimeLike[Time] {
    def now() = fromMilliseconds(System.currentTimeMillis())

    val Epoch = new Time(0)

    override val Max = new Time(Long.MaxValue)
    override val Min = new Time(Long.MinValue)
    def fromMilliseconds(ms: Long) = new Time(ms)
}

class TimeFormat(pattern: String) {
    import java.util.TimeZone
    import java.text.SimpleDateFormat
    private val format = new SimpleDateFormat(pattern)
    format.setTimeZone(TimeZone.getTimeZone("UTC"))

    def format(time: Time): String = format.format(time.toDate)
}

/** Useful for time sensitive unit tests.
 *  Warning! This is _not_ thread safe! It is only intended for single-thread unit tests.
 */
object FakeTime extends FakeTimeInstance

class FakeTimeInstance extends TimeSource {
    import java.util.concurrent.atomic.AtomicLong
    val current = new AtomicLong(System.currentTimeMillis())

    override def now() = Time.fromMilliseconds(current.get)

    def set(now: Time) { set(now.milliseconds) }
    def set(now: Long) { current.set(now) }

    def +(delta: Duration) = add(delta)
    def add(delta: Duration) = current.set(now.milliseconds + delta.milliseconds)
}