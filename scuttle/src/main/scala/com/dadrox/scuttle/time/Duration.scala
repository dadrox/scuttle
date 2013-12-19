package com.dadrox.scuttle.time

object Duration extends DurationSource[Duration] {
    object Millis {
        val perSecond = 1000L
        val perMinute = perSecond * 60L
        val perHour = perMinute * 60L
        val perDay = perHour * 24L
        val perWeek = perDay * 7L
    }

    def apply(milliseconds: Long) =
        if(milliseconds >= MaxMilliseconds) Infinite
        else if(milliseconds <= MinMilliseconds) NegativeInfinite
        else FiniteDuration(milliseconds)

    lazy val Infinite = Infinity
    lazy val NegativeInfinite = NegativeInfinity
}

private[time] case object Infinity extends Duration {
    def milliseconds = ops.MaxMilliseconds

    override def abs: Duration = this
    override def unary_-(): Duration = NegativeInfinity
    override def *(scalar: Long): Duration = if(scalar < 0) NegativeInfinity else this
    override def /(scalar: Long): Duration = if(scalar < 0) NegativeInfinity else this
    override def %(scalar: Long): Duration = this
    override def finite_?(): Boolean = false

    override def +(other: Duration): Duration = this
    override def -(other: Duration): Duration = this

    override val toString = "InfinityDuration"
}

private[time] case object NegativeInfinity extends Duration {
    def milliseconds = ops.MinMilliseconds

    override def abs: Duration = Infinity
    override def unary_-(): Duration = Infinity
    override def *(scalar: Long): Duration = if(scalar < 0) Infinity else this
    override def /(scalar: Long): Duration = if(scalar < 0) Infinity else this
    override def %(scalar: Long): Duration = this
    override def finite_?(): Boolean = false

    override def +(other: Duration): Duration = this
    override def -(other: Duration): Duration = this

    override val toString = "NegativeInfinityDuration"
}

trait Duration extends DurationInstance[Duration] {
    override val ops = Duration

    def abs: Duration = if (milliseconds < 0) -this else ops(inMilliseconds)
    def unary_-(): Duration = ops(-inMilliseconds)
    def /(scalar: Long): Duration = ops(inMilliseconds / scalar)
    def %(scalar: Long): Duration = ops(inMilliseconds % scalar)
    def finite_?(): Boolean = true

    def *(scalar: Long): Duration = {
        import java.lang.Long.{ numberOfLeadingZeros => leadingZeroes }
        val ms = math.abs(milliseconds)
        val s = math.abs(scalar)
        if (leadingZeroes(ms) + leadingZeroes(s) < 64) {
            (milliseconds < 0, scalar < 0) match {
                case (true, false) | (false, true) => ops.NegativeInfinite
                case _                             => ops.Infinite
            }
        } else {
            val product = ms * s
            if (product < 0) ops.NegativeInfinite
            else if (ms == milliseconds ^ s == scalar) ops(-product)
            else ops(product)
        }
    }

    override def +(other: Duration): Duration = {
        val ms = inMilliseconds
        val otherMs = other.inMilliseconds
        if (otherMs > 0 && ms > Long.MaxValue - otherMs) ops.Infinite
        else if (otherMs < 0 && ms < -Long.MaxValue - otherMs) ops.NegativeInfinite
        else ops.fromMilliseconds(ms + otherMs)
    }

    override val toString: String = toString(terse = false)

    def toString(
        maxTimeUnit: TimeUnit.EnumVal = TimeUnit.Week,
        terse: Boolean = false) = {
        var remainder = math.abs(if (milliseconds == Long.MinValue) milliseconds + 1 else milliseconds)
        val sign = if (milliseconds < 0) "-" else "+"
        val sb = new StringBuilder
        for (
            u <- TimeUnit.values.flatMap { d => if (d.msPer > maxTimeUnit.msPer) None else Some(d) }
        ) {
            val dividend = remainder / u.msPer
            if (dividend > 0) {
                sb.append(sign + dividend + "." + (if (terse) u.short else u.name))
                remainder -= dividend * u.msPer
                if (!terse && dividend > 1) sb.append("s")
            }
        }
        sb.toString
    }
}

case class FiniteDuration private[time] (milliseconds: Long) extends Duration with DurationInstance[Duration]

trait DurationSource[A <: DurationInstance[A]] {
    def apply(milliseconds: Long): A // abstract

    val MaxMilliseconds = Long.MaxValue
    val MinMilliseconds = -MaxMilliseconds

    def fromMilliseconds(ms: Long): A = apply(ms)
    def fromSeconds(s: Long): A = fromMilliseconds(s * Duration.Millis.perSecond)
    def fromMinutes(m: Long): A = fromMilliseconds(m * Duration.Millis.perMinute)
    def fromHours(h: Long): A = fromMilliseconds(h * Duration.Millis.perHour)
    def fromDays(d: Long): A = fromMilliseconds(d * Duration.Millis.perDay)
    def fromWeeks(w: Long): A = fromMilliseconds(w * Duration.Millis.perWeek)
}

trait DurationInstance[A <: DurationInstance[A]] extends Ordered[A] {
    import Duration._
    protected def ops: DurationSource[A]

    def milliseconds: Long // abstract

    def inMilliseconds = milliseconds
    def inMillis: Long = inMilliseconds
    def inMs: Long = inMilliseconds

    def inSeconds: Int = (inMilliseconds / Millis.perSecond).toInt
    def inMinutes: Int = (inMilliseconds / Millis.perMinute).toInt
    def inHours: Int = (inMilliseconds / Millis.perHour).toInt
    def inDays: Int = (inMilliseconds / Millis.perDay).toInt
    def inWeeks: Int = (inMilliseconds / Millis.perWeek).toInt

    def +(other: Duration): A = ops.fromMilliseconds(inMilliseconds + other.inMilliseconds)
    def -(other: Duration): A = this + -other

    override def compare(other: A): Int = inMilliseconds compare other.inMilliseconds
}