package com.dadrox.scuttle.time

object Duration extends DurationSource[Duration] {
    def apply(milliseconds: Long) =
        if (milliseconds >= MaxMilliseconds) Infinite
        else if (milliseconds <= MinMilliseconds) NegativeInfinite
        else FiniteDuration(milliseconds)

    lazy val Infinite = Infinity
    lazy val NegativeInfinite = NegativeInfinity
}

trait Inf extends Duration {
    override def abs: Duration = Infinity
    override def finite_?(): Boolean = false

    override def *(scalar: Long): Duration = flip(scalar)
    override def /(scalar: Long): Duration = flip(scalar)
    private def flip(scalar: Long) = if (scalar < 0) -this else this

    override def %(scalar: Long): Duration = this
    override def +(other: Duration): Duration = this
    override def -(other: Duration): Duration = this

}

private[time] case object Infinity extends Inf {
    def milliseconds = ops.MaxMilliseconds
    override val toString = "InfinityDuration"
}

private[time] case object NegativeInfinity extends Inf {
    def milliseconds = ops.MinMilliseconds
    override val toString = "NegativeInfinityDuration"
}

trait Duration extends DurationInstance[Duration] {
    override val ops = Duration

    def abs: Duration = if (milliseconds < 0) -this else this
    def unary_-(): Duration = ops(-milliseconds)
    def /(scalar: Long): Duration = ops(milliseconds / scalar)
    def %(scalar: Long): Duration = ops(milliseconds % scalar)
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
        val ms = milliseconds
        val otherMs = other.milliseconds
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

        def appendString(value: Long, unit: TimeUnit.EnumVal) = {
            sb.append(sign + value + "." + (if (terse) unit.short else unit.name))
            remainder -= value * unit.ms
            if (!terse && (value > 1 || value == 0)) sb.append("s")
        }

        if (milliseconds == 0) appendString(0, TimeUnit.Millisecond)
        else for (
            unit <- TimeUnit.values.flatMap { d => if (d.ms > maxTimeUnit.ms) None else Some(d) }
        ) {
            val dividend = remainder / unit.ms
            if (dividend > 0) {
                appendString(dividend, unit)
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

    import TimeUnit._
    def fromMilliseconds(ms: Long): A = apply(ms)
    def fromSeconds(s: Long): A = fromMilliseconds(s * Second.ms)
    def fromMinutes(m: Long): A = fromMilliseconds(m * Minute.ms)
    def fromHours(h: Long): A = fromMilliseconds(h * Hour.ms)
    def fromDays(d: Long): A = fromMilliseconds(d * Day.ms)
    def fromWeeks(w: Long): A = fromMilliseconds(w * Week.ms)
}

trait DurationInstance[A <: DurationInstance[A]] extends Ordered[A] {
    import TimeUnit._
    protected def ops: DurationSource[A]

    def milliseconds: Long // abstract

    def inMilliseconds = milliseconds
    def inMillis: Long = milliseconds
    def inMs: Long = milliseconds

    def inSeconds: Int = (milliseconds / Second.ms).toInt
    def inMinutes: Int = (milliseconds / Minute.ms).toInt
    def inHours: Int = (milliseconds / Hour.ms).toInt
    def inDays: Int = (milliseconds / Day.ms).toInt
    def inWeeks: Int = (milliseconds / Week.ms).toInt

    def +(other: Duration): A = ops.fromMilliseconds(milliseconds + other.milliseconds)
    def -(other: Duration): A = this + -other

    override def compare(other: A): Int = milliseconds compare other.milliseconds
}