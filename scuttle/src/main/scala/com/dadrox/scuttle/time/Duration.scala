package com.dadrox.scuttle.time

object Duration extends DurationSource[Duration] {
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

case class Duration private[time] (milliseconds: Long) extends DurationInstance[Duration] {
    override val ops = Duration
    def inMilliseconds() = milliseconds

    def abs(): Duration = if (milliseconds < 0) -this else this
    def unary_-(): Duration = Duration(-inMilliseconds)
    def *(scalar: Int): Duration = Duration(inMilliseconds * scalar)
    def *(scalar: Long): Duration = Duration(inMilliseconds * scalar)
    def /(scalar: Int): Duration = Duration(inMilliseconds / scalar)
    def /(scalar: Long): Duration = Duration(inMilliseconds / scalar)
    def %(scalar: Int): Duration = Duration(inMilliseconds % scalar)
    def %(scalar: Long): Duration = Duration(inMilliseconds % scalar)

    override val toString: String = toString(false)

    def toString(terse: Boolean = false): String = {
        val sign = if (milliseconds < 0) "-" else "+"
        val sb = new StringBuilder
        var remainder = math.abs(milliseconds)
        for (u <- TimeUnit.values) {
            val dividend = remainder / u.msPer
            if (dividend > 0) {
                sb.append(sign + dividend + "." + (if (terse) u.short else u.name))
                if (dividend > 1) sb.append("s")
                remainder -= dividend * u.msPer
            }
        }
        sb.toString
    }
}

trait DurationSource[A <: DurationInstance[A]] {
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

trait DurationInstance[A <: DurationInstance[A]] extends Ordered[A] {
    import Duration._
    protected def ops(): DurationSource[A]

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