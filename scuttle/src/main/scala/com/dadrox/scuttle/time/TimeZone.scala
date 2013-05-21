package com.dadrox.scuttle.time

import com.dadrox.scuttle.Enum

object TimeZone extends Enum {
    sealed case class EnumVal private[TimeZone] (
        name: String,
        canonical: String,
        standardOffset: String,
        aliases: List[String] = Nil)
            extends Value

}

class TimeZone {

}