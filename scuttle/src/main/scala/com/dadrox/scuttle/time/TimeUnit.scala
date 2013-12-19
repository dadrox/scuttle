package com.dadrox.scuttle.time

import com.dadrox.scuttle.Enum

object TimeUnit extends Enum {
    sealed case class EnumVal private[TimeUnit] (name: String, short: String, ms: Long) extends Value

    val Week = EnumVal("week", "w", 7 * 24 * 60 * 60 * 1000L)
    val Day = EnumVal("day", "d", 24 * 60 * 60 * 1000L)
    val Hour = EnumVal("hour", "h", 60 * 60 * 1000L)
    val Minute = EnumVal("minute", "m", 60 * 1000L)
    val Second = EnumVal("second", "s", 1000L)
    val Millisecond = EnumVal("millisecond", "ms", 1L)
}