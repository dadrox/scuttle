package com.dadrox.scuttle

import org.fictus.Fictus
import com.dadrox.scuttle.string.converters
import com.dadrox.scuttle.string._

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
        "+3".asLong mustEqual Some(3L)
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
        "+3".asInt mustEqual Some(3)
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
        BigDecimal(Float.MaxValue).*(2).toString.asFloat mustEqual None
        BigDecimal(Float.MaxValue).*(-2).toString.asFloat mustEqual None
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
        BigDecimal(Double.MaxValue).*(2).toString.asDouble mustEqual None
        BigDecimal(Double.MaxValue).*(-2).toString.asDouble mustEqual None
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

    @Test
    def extractor_IsInt {
        nullString mustNotMatch { case IsInt(i) => }
        "" mustNotMatch { case IsInt(i) => }
        " " mustNotMatch { case IsInt(i) => }
        "a" mustNotMatch { case IsInt(i) => }
        "3." mustNotMatch { case IsInt(i) => }
        BigInt(Int.MaxValue).+(1).toString mustNotMatch { case IsInt(i) => }
        BigInt(Int.MinValue).-(1).toString mustNotMatch { case IsInt(i) => }

        "-1" mustMatch { case IsInt(-1) => }
        "00" mustMatch { case IsInt(0) => }
        "3" mustMatch { case IsInt(3) => }
        "+3" mustMatch { case IsInt(3) => }
        "-3" mustMatch { case IsInt(-3) => }
        Int.MaxValue.toString mustMatch { case IsInt(Int.MaxValue) => }
        Int.MinValue.toString mustMatch { case IsInt(Int.MinValue) => }
    }

    @Test
    def extractor_IsLong {
        nullString mustNotMatch { case IsLong(i) => }
        "" mustNotMatch { case IsLong(i) => }
        " " mustNotMatch { case IsLong(i) => }
        "a" mustNotMatch { case IsLong(i) => }
        "3." mustNotMatch { case IsLong(i) => }
        BigInt(Long.MaxValue).+(1).toString mustNotMatch { case IsLong(i) => }
        BigInt(Long.MinValue).-(1).toString mustNotMatch { case IsLong(i) => }

        "-1" mustMatch { case IsLong(-1) => }
        "00" mustMatch { case IsLong(0) => }
        "3" mustMatch { case IsLong(3) => }
        "+3" mustMatch { case IsLong(3) => }
        "-3" mustMatch { case IsLong(-3) => }
        Long.MaxValue.toString mustMatch { case IsLong(Long.MaxValue) => }
        Long.MinValue.toString mustMatch { case IsLong(Long.MinValue) => }
    }

    @Test
    def extractor_IsShort {
        nullString mustNotMatch { case IsShort(i) => }
        "" mustNotMatch { case IsShort(i) => }
        " " mustNotMatch { case IsShort(i) => }
        "a" mustNotMatch { case IsShort(i) => }
        "3." mustNotMatch { case IsShort(i) => }
        BigInt(Short.MaxValue).+(1).toString mustNotMatch { case IsShort(i) => }
        BigInt(Short.MinValue).-(1).toString mustNotMatch { case IsShort(i) => }

        "-1" mustMatch { case IsShort(-1) => }
        "00" mustMatch { case IsShort(0) => }
        "3" mustMatch { case IsShort(3) => }
        "+3" mustMatch { case IsShort(3) => }
        "-3" mustMatch { case IsShort(-3) => }
        Short.MaxValue.toString mustMatch { case IsShort(Short.MaxValue) => }
        Short.MinValue.toString mustMatch { case IsShort(Short.MinValue) => }
    }

    @Test
    def extractor_IsByte {
        nullString mustNotMatch { case IsByte(i) => }
        "" mustNotMatch { case IsByte(i) => }
        " " mustNotMatch { case IsByte(i) => }
        "a" mustNotMatch { case IsByte(i) => }
        "3." mustNotMatch { case IsByte(i) => }
        BigInt(Byte.MaxValue).+(1).toString mustNotMatch { case IsByte(i) => }
        BigInt(Byte.MinValue).-(1).toString mustNotMatch { case IsByte(i) => }

        "-1" mustMatch { case IsByte(-1) => }
        "00" mustMatch { case IsByte(0) => }
        "3" mustMatch { case IsByte(3) => }
        "+3" mustMatch { case IsByte(3) => }
        "-3" mustMatch { case IsByte(-3) => }
        Byte.MaxValue.toString mustMatch { case IsByte(Byte.MaxValue) => }
        Byte.MinValue.toString mustMatch { case IsByte(Byte.MinValue) => }
    }

    @Test
    def extractor_IsFloat {
        "0.0" mustMatch { case IsFloat(0.0) => }
        "0" mustMatch { case IsFloat(0) => }
        "-1" mustMatch { case IsFloat(-1.0) => }
        "3" mustMatch { case IsFloat(3) => }
        "3.0" mustMatch { case IsFloat(3.0) => }
        "+3.0" mustMatch { case IsFloat(3.0) => }
        "-3.0" mustMatch { case IsFloat(-3.0) => }
        "0.3" mustMatch { case IsFloat(0.3f) => }
        ".3" mustMatch { case IsFloat(0.3f) => }
        Float.MaxValue.toString mustMatch { case IsFloat(Float.MaxValue) => }
        Float.MinValue.toString mustMatch { case IsFloat(Float.MinValue) => }

        nullString mustNotMatch { case IsFloat(i) => }
        "" mustNotMatch { case IsFloat(i) => }
        "." mustNotMatch { case IsFloat(i) => }
        "E" mustNotMatch { case IsFloat(i) => }
        "3." mustNotMatch { case IsFloat(i) => }
        " " mustNotMatch { case IsFloat(i) => }
        "a" mustNotMatch { case IsFloat(i) => }
        BigDecimal(Float.MaxValue).*(2).toString mustNotMatch { case IsFloat(_) => }
        BigDecimal(Float.MaxValue).*(-2).toString mustNotMatch { case IsFloat(_) => }
    }

    @Test
    def extractor_IsDouble {
        "0.0" mustMatch { case IsDouble(0.0) => }
        "0" mustMatch { case IsDouble(0) => }
        "-1" mustMatch { case IsDouble(-1.0) => }
        "3" mustMatch { case IsDouble(3) => }
        "3.0" mustMatch { case IsDouble(3.0) => }
        "+3.0" mustMatch { case IsDouble(3.0) => }
        "-3.0" mustMatch { case IsDouble(-3.0) => }
        "0.3" mustMatch { case IsDouble(0.3) => }
        ".3" mustMatch { case IsDouble(0.3) => }
        Double.MaxValue.toString mustMatch { case IsDouble(Double.MaxValue) => }
        Double.MinValue.toString mustMatch { case IsDouble(Double.MinValue) => }

        nullString mustNotMatch { case IsDouble(i) => }
        "" mustNotMatch { case IsDouble(i) => }
        "." mustNotMatch { case IsDouble(i) => }
        "E" mustNotMatch { case IsDouble(i) => }
        "3." mustNotMatch { case IsDouble(i) => }
        " " mustNotMatch { case IsDouble(i) => }
        "a" mustNotMatch { case IsDouble(i) => }
        BigDecimal(Double.MaxValue).*(2).toString mustNotMatch { case IsDouble(_) => }
        BigDecimal(Double.MaxValue).*(-2).toString mustNotMatch { case IsDouble(_) => }
    }
}