package com.dadrox.scuttle.time

import com.dadrox.scuttle.Enum

object Duration extends DurationLike[Duration] {
    object Millis {
        val perSecond = 1000L
        val perMinute = perSecond * 60L
        val perHour = perMinute * 60L
        val perDay = perHour * 24L
        val perWeek = perDay * 7L
    }

    def Max = new Duration(Long.MaxValue)
    def Min = new Duration(Long.MinValue)
    def fromMilliseconds(ms: Long) = new Duration(ms)
}

case class Duration private[time] (milliseconds: Long) extends DurationLikeOps[Duration] {
    override val ops = Duration
    def inMilliseconds() = milliseconds

    def *(scalar: Int): Duration = Duration(inMilliseconds * scalar)
    def *(scalar: Long): Duration = Duration(inMilliseconds * scalar)
    def /(scalar: Int): Duration = Duration(inMilliseconds / scalar)
    def /(scalar: Long): Duration = Duration(inMilliseconds / scalar)

    override val toString: String = toString(false)
    def toString(terse: Boolean = false): String = {
        def padded(w: Int, value: Long) = "0" * (w - value.toString.length()) + value
        def div(numerator: Long, denominator: Long): (Long, Long) = (numerator / denominator, numerator % denominator)

        val sb = new StringBuilder
        var remainder = milliseconds
        for (u <- TimeUnit.values) {
            val dividend = remainder / u.msPer
            if (dividend > 0) {
                sb.append("+" + dividend + "." + u.name)
                if (dividend > 1) sb.append("s")
            }
            remainder -= dividend * u.msPer
        }
        sb.toString
    }
}

object TimeUnit extends Enum {
    sealed case class EnumVal private[TimeUnit] (name: String, short: String, msPer: Long) extends Value

    val Week = EnumVal("week", "w", 7 * 24 * 60 * 60 * 1000L)
    val Day = EnumVal("day", "d", 24 * 60 * 60 * 1000L)
    val Hour = EnumVal("hour", "h", 60 * 60 * 1000L)
    val Minute = EnumVal("minute", "m", 60 * 1000L)
    val Second = EnumVal("second", "s", 1000L)
    val Millisecond = EnumVal("millisecond", "ms", 1L)
}

trait DurationLike[A <: DurationLikeOps[A]] {
    // TODO Add positive and negative infinity everywhere to deal with over/underflows

    /** The largest possible Duration
     */
    def Max(): A

    /** The smallest possible Duration
     */
    def Min(): A

    def fromMilliseconds(ms: Long): A
    def fromSeconds(s: Long): A = fromMilliseconds(s * Duration.Millis.perSecond)
    def fromMinutes(m: Long): A = fromMilliseconds(m * Duration.Millis.perMinute)
    def fromHours(h: Long): A = fromMilliseconds(h * Duration.Millis.perHour)
    def fromDays(d: Long): A = fromMilliseconds(d * Duration.Millis.perDay)
    def fromWeeks(w: Long): A = fromMilliseconds(w * Duration.Millis.perWeek)
}

trait DurationLikeOps[A <: DurationLikeOps[A]] extends Ordered[A] {
    import Duration._
    protected def ops(): DurationLike[A]

    def inMilliseconds(): Long // abstract
    def inMillis(): Long = inMilliseconds
    def inMs(): Long = inMilliseconds

    def inSeconds(): Int = (inMilliseconds / Millis.perSecond).toInt
    def inMinutes(): Int = (inMilliseconds / Millis.perMinute).toInt
    def inHours(): Int = (inMilliseconds / Millis.perHour).toInt
    def inDays(): Int = (inMilliseconds / Millis.perDay).toInt
    def inWeeks(): Int = (inMilliseconds / Millis.perWeek).toInt

    def +(other: Duration): A = ops.fromMilliseconds(inMilliseconds + other.inMilliseconds)
    def -(other: Duration): A = ops.fromMilliseconds(inMilliseconds - other.inMilliseconds)

    override def compare(other: A): Int = inMilliseconds compare other.inMilliseconds
}