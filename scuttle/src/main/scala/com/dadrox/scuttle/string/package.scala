package com.dadrox.scuttle

package object string {

    def hr(c: String, width: Int): String = c * width

    object IsInt {
        def unapply(s: String): Option[Int] = s.asInt
    }

    object IsLong {
        def unapply(s: String): Option[Long] = s.asLong
    }

    object IsShort {
        def unapply(s: String): Option[Short] = s.asShort
    }

    object IsByte {
        def unapply(s: String): Option[Byte] = s.asByte
    }

    object IsBoolean {
        def unapply(s: String): Option[Boolean] = s.asBoolean
    }

    object IsFloat {
        def unapply(s: String): Option[Float] = s.asFloat
    }

    object IsDouble {
        def unapply(s: String): Option[Double] = s.asDouble
    }

    private lazy val truths = Vector("true", "t", "yes", "y", "1")
    private lazy val falses = Vector("false", "f", "no", "n", "0")

    private lazy val Integer = """([+-])?(\d+)""".r
    private lazy val FloatingPoint = """([-+]?\d*\.?\d+)([eE][-+]?\d+)?""".r

    /** Provides various functions to convert a String to something else safely (i.e. Options, not exceptions) */
    implicit class StringToValueConverters(val s: String) extends AnyVal {

        def notNull: Option[String] = Option(s)

        def notEmpty: Option[String] = s match {
            case "" => None
            case _  => notNull
        }

        def notBlank: Option[String] = s.notEmpty.flatMap(_ => s.trim.notEmpty)

        private def asInteger(): Option[BigInt] = s match {
            case Integer("-", i)     => Some(BigInt("-" + i))
            case Integer("+" | _, i) => Some(BigInt(i))
            case _                   => None
        }

        def asInt(): Option[Int] = asInteger.flatMap {
            case i if (i <= Int.MaxValue && i >= Int.MinValue) => Some(i.toInt)
            case _                                             => None
        }

        def asLong(): Option[Long] = asInteger.flatMap {
            case i if (i <= Long.MaxValue && i >= Long.MinValue) => Some(i.toLong)
            case _                                               => None
        }

        def asShort(): Option[Short] = asInteger.flatMap {
            case i if (i <= Short.MaxValue && i >= Short.MinValue) => Some(i.toShort)
            case _                                                 => None
        }

        def asByte(): Option[Byte] = asInteger.flatMap {
            case i if (i <= Byte.MaxValue && i >= Byte.MinValue) => Some(i.toByte)
            case _                                               => None
        }

        def asFloatingPoint(): Option[BigDecimal] = s match {
            case FloatingPoint(i, null) => Some(BigDecimal(i))
            case FloatingPoint(i, e)    => Some(BigDecimal(i + e.toUpperCase()))
            case miss                   => None
        }

        def asFloat(): Option[Float] = asFloatingPoint.flatMap {
            case i if (i <= BigDecimal(Float.MaxValue.toString) && i >= BigDecimal(Float.MinValue.toString)) => Some(i.floatValue)
            case miss => None
        }

        def asDouble(): Option[Double] = asFloatingPoint.flatMap {
            case i if (i <= BigDecimal(Double.MaxValue.toString) && i >= BigDecimal(Double.MinValue.toString)) => Some(i.doubleValue)
            case _ => None
        }

        def asBoolean(): Option[Boolean] = s.notBlank flatMap {
            case b if (truths.exists(_.equalsIgnoreCase(b))) => Some(true)
            case b if (falses.exists(_.equalsIgnoreCase(b))) => Some(false)
            case _                                           => None
        }
    }

    implicit class StringWithUtilitiesConverters(s: String) {
        private def nullSafe = if (s == null) "" else s
        private def padIt(width: Int, padChar: Char) = padChar.toString * (width - nullSafe.length())

        object pad {
            object left {
                def apply(width: Int, padChar: Char): String = padIt(width, padChar) + nullSafe
                def zero(width: Int): String = apply(width, '0')
            }
            object right {
                def apply(width: Int, padChar: Char): String = nullSafe + padIt(width, padChar)
                def zero(width: Int): String = apply(width, '0')
            }
        }

        def hr(width: Int): String = string.hr(s, width)

        def framed(frame: Char): String = {
            val lines = s.lines.toVector
            val maxLineLength = lines.map(_.length()).max
            val hr = frame.toString * (maxLineLength + 4)
            s"""
            |$hr
            |${lines.map(line => s"$frame ${line.pad.right(maxLineLength, ' ')} $frame").mkString("\n")}
            |$hr""".stripMargin
        }

        lazy val framed: String = framed('*')
    }
}