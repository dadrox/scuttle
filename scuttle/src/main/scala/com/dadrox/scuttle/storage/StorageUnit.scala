package com.dadrox.scuttle.storage

import com.dadrox.scuttle.Enum

object StorageUnit extends Enum {
    case class EnumVal private[StorageUnit] (name: String, short: String, base: Int, bytes: Long) extends Value

    val ByteBase10 = EnumVal("byte", "B", 10, 1)
    val Kilobyte = EnumVal("kilobyte", "kB", 10, 1000L)
    val Megabyte = EnumVal("megabyte", "MB", 10, 1000L * 1000)
    val Gigabyte = EnumVal("gigabyte", "GB", 10, 1000L * 1000 * 1000)
    val Terabyte = EnumVal("terabyte", "TB", 10, 1000L * 1000 * 1000 * 1000)
    val Petabyte = EnumVal("petabyte", "PB", 10, 1000L * 1000 * 1000 * 1000 * 1000)
    val Exabyte = EnumVal("exabyte", "EB", 10, 1000L * 1000 * 1000 * 1000 * 1000 * 1000)

    val ByteBase2 = EnumVal("byte", "B", 2, 1)
    val Kibibyte = EnumVal("kibibyte", "KiB", 2, 1024L)
    val Mebibyte = EnumVal("mebibyte", "MiB", 2, 1024L * 1024)
    val Gibibyte = EnumVal("gibibyte", "GiB", 2, 1024L * 1024 * 1024)
    val Tebibyte = EnumVal("tebibyte", "TiB", 2, 1024L * 1024 * 1024 * 1024)
    val Pebibyte = EnumVal("pebibyte", "PiB", 2, 1024L * 1024 * 1024 * 1024 * 1024)
    val Exbibyte = EnumVal("exbibyte", "EiB", 2, 1024L * 1024 * 1024 * 1024 * 1024 * 1024)
}