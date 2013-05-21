package com.dadrox.scuttle

import org.junit.Test
import org.fictus.Fictus

import com.dadrox.scuttle.string.converters;

class StringConvertersTest extends Fictus {

    val nullString: String = null

    @Test
    def notNull {
        nullString.notNull mustEqual None
        "".notNull mustEqual Some("")
        "null".notNull mustEqual Some("null")
        "a".notNull mustEqual Some("a")
    }

    @Test
    def notEmpty {
        nullString.notEmpty mustEqual None
        "".notEmpty mustEqual None
        " ".notEmpty mustEqual Some(" ")
        "   ".notEmpty mustEqual Some("   ")
        "a".notEmpty mustEqual Some("a")
    }

    @Test
    def notBlank {
        nullString.notBlank mustEqual None
        "".notBlank mustEqual None
        " ".notBlank mustEqual None
        "   ".notBlank mustEqual None
        "a".notBlank mustEqual Some("a")
    }

    @Test
    def asLong {
        "0".asLong mustEqual Some(0L)
        "-1".asLong mustEqual Some(-1L)
        "3".asLong mustEqual Some(3L)
        Long.MaxValue.toString.asLong mustEqual Some(Long.MaxValue)
        Long.MinValue.toString.asLong mustEqual Some(Long.MinValue)

        nullString.asLong mustEqual None
        "".asLong mustEqual None
        " ".asLong mustEqual None
        "a".asLong mustEqual None
        "3.".asLong mustEqual None
        BigInt(Long.MaxValue).+(1).toString.asLong mustEqual None
        BigInt(Long.MinValue).-(1).toString.asLong mustEqual None
    }

    @Test
    def asInt {
        "0".asInt mustEqual Some(0)
        "-1".asInt mustEqual Some(-1)
        "3".asInt mustEqual Some(3)
        Int.MaxValue.toString.asInt mustEqual Some(Int.MaxValue)
        Int.MinValue.toString.asInt mustEqual Some(Int.MinValue)

        nullString.asInt mustEqual None
        "".asInt mustEqual None
        " ".asInt mustEqual None
        "a".asInt mustEqual None
        "3.".asInt mustEqual None
        BigInt(Int.MaxValue.toLong).+(1).toString.asInt mustEqual None
        BigInt(Int.MinValue.toLong).-(1).toString.asInt mustEqual None
    }

    @Test
    def asShort {
        "0".asShort mustEqual Some(0: Short)
        "-1".asShort mustEqual Some(-1: Short)
        "3".asShort mustEqual Some(3: Short)
        Short.MaxValue.toString.asShort mustEqual Some(Short.MaxValue)
        Short.MinValue.toString.asShort mustEqual Some(Short.MinValue)

        nullString.asShort mustEqual None
        "".asShort mustEqual None
        " ".asShort mustEqual None
        "a".asShort mustEqual None
        "3.".asShort mustEqual None
        BigInt(Short.MaxValue).+(1).toString.asShort mustEqual None
        BigInt(Short.MinValue).-(1).toString.asShort mustEqual None
    }

    @Test
    def asByte {
        "0".asByte mustEqual Some(0: Byte)
        "-1".asByte mustEqual Some(-1: Byte)
        "3".asByte mustEqual Some(3: Byte)
        Byte.MaxValue.toString.asByte mustEqual Some(Byte.MaxValue)
        Byte.MinValue.toString.asByte mustEqual Some(Byte.MinValue)

        nullString.asByte mustEqual None
        "".asByte mustEqual None
        " ".asByte mustEqual None
        "a".asByte mustEqual None
        "3.".asByte mustEqual None
        BigInt(Byte.MaxValue).+(1).toString.asByte mustEqual None
        BigInt(Byte.MinValue).-(1).toString.asByte mustEqual None
    }

    @Test
    def asFloat {
        "0.0".asFloat mustEqual Some(0.0f)
        "0".asFloat mustEqual Some(0f)
        "-1".asFloat mustEqual Some(-1f)
        "3".asFloat mustEqual Some(3f)
        "3.0".asFloat mustEqual Some(3f)
        "+3.0".asFloat mustEqual Some(3f)
        "-3.0".asFloat mustEqual Some(-3f)
        "0.3".asFloat mustEqual Some(0.3f)
        ".3".asFloat mustEqual Some(0.3f)
        Float.MaxValue.toString.asFloat mustEqual Some(Float.MaxValue)
        Float.MinValue.toString.asFloat mustEqual Some(Float.MinValue)

        nullString.asFloat mustEqual None
        "".asFloat mustEqual None
        ".".asFloat mustEqual None
        "E".asFloat mustEqual None
        "3.".asFloat mustEqual None
        " ".asFloat mustEqual None
        "a".asFloat mustEqual None
    }

    @Test
    def asDouble {
        "0.0".asDouble mustEqual Some(0.0)
        "0".asDouble mustEqual Some(0.0)
        "-1".asDouble mustEqual Some(-1.0)
        "3".asDouble mustEqual Some(3.0)
        "3.0".asDouble mustEqual Some(3.0)
        "+3.0".asDouble mustEqual Some(3.0)
        "-3.0".asDouble mustEqual Some(-3.0)
        "0.3".asDouble mustEqual Some(0.3)
        ".3".asDouble mustEqual Some(0.3)
        Double.MaxValue.toString.asDouble mustEqual Some(Double.MaxValue)
        Double.MinValue.toString.asDouble mustEqual Some(Double.MinValue)

        nullString.asDouble mustEqual None
        "".asDouble mustEqual None
        ".".asDouble mustEqual None
        "E".asDouble mustEqual None
        "3.".asDouble mustEqual None
        " ".asDouble mustEqual None
        "a".asDouble mustEqual None
    }

    @Test
    def asBoolean {
        "true".asBoolean mustEqual Some(true)
        "True".asBoolean mustEqual Some(true)
        "TRUE".asBoolean mustEqual Some(true)
        "t".asBoolean mustEqual Some(true)
        "T".asBoolean mustEqual Some(true)
        "y".asBoolean mustEqual Some(true)
        "Y".asBoolean mustEqual Some(true)
        "yes".asBoolean mustEqual Some(true)
        "YES".asBoolean mustEqual Some(true)
        "1".asBoolean mustEqual Some(true)

        "false".asBoolean mustEqual Some(false)
        "False".asBoolean mustEqual Some(false)
        "FALSE".asBoolean mustEqual Some(false)
        "f".asBoolean mustEqual Some(false)
        "F".asBoolean mustEqual Some(false)
        "n".asBoolean mustEqual Some(false)
        "N".asBoolean mustEqual Some(false)
        "no".asBoolean mustEqual Some(false)
        "NO".asBoolean mustEqual Some(false)
        "0".asBoolean mustEqual Some(false)

        nullString.asBoolean mustEqual None
        "".asBoolean mustEqual None
    }
}