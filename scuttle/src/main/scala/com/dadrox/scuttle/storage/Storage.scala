package com.dadrox.scuttle.storage

object Storage {
    import StorageUnit._

    def fromBytes(bytes: Long) = apply(bytes)

    def fromKilobytes(in: Long) = fromBytes(in * Kilobyte.bytes)
    def fromMegabytes(in: Long) = fromBytes(in * Megabyte.bytes)
    def fromGigabytes(in: Long) = fromBytes(in * Gigabyte.bytes)
    def fromTerabytes(in: Long) = fromBytes(in * Terabyte.bytes)
    def fromPetabytes(in: Long) = fromBytes(in * Petabyte.bytes)
    def fromExabytes(in: Long) = fromBytes(in * Exabyte.bytes)

    def fromKibibytes(in: Long) = fromBytes(in * Kibibyte.bytes)
    def fromMebibytes(in: Long) = fromBytes(in * Mebibyte.bytes)
    def fromGibibytes(in: Long) = fromBytes(in * Gibibyte.bytes)
    def fromTebibytes(in: Long) = fromBytes(in * Tebibyte.bytes)
    def fromPebibytes(in: Long) = fromBytes(in * Pebibyte.bytes)
    def fromExbibytes(in: Long) = fromBytes(in * Exbibyte.bytes)
}

case class Storage private (bytes: Long) extends Ordered[Storage] {
    import StorageUnit._

    def inBytes = bytes

    def inKilobytes = bytes.toDouble / Kilobyte.bytes
    def inMegabytes = bytes.toDouble / Megabyte.bytes
    def inGigabytes = bytes.toDouble / Gigabyte.bytes
    def inTerabytes = bytes.toDouble / Terabyte.bytes
    def inPetabytes = bytes.toDouble / Petabyte.bytes
    def inExabytes = bytes.toDouble / Exabyte.bytes

    def inKibibytes = bytes.toDouble / Kibibyte.bytes
    def inMebibytes = bytes.toDouble / Mebibyte.bytes
    def inGibibytes = bytes.toDouble / Gibibyte.bytes
    def inTebibytes = bytes.toDouble / Tebibyte.bytes
    def inPebibytes = bytes.toDouble / Pebibyte.bytes
    def inExbibytes = bytes.toDouble / Exbibyte.bytes

    def *(scalar: Double) = Storage((bytes.toDouble * scalar).toLong)
    def /(scalar: Double) = Storage((bytes.toDouble / scalar).toLong)
    def +(other: Storage) = Storage(bytes + other.bytes)
    def -(other: Storage) = Storage(bytes - other.bytes)


    // TODO
//    def abs
    def floor(s: Storage) = Storage.fromBytes(bytes / s.bytes * s.bytes)
    def ceiling(s: Storage) = if (bytes % s.bytes == 0) floor(s) else floor(s) + s.bytes.bytes

    override def compare(other: Storage): Int = bytes compare other.bytes

    override val toString = s"$bytes.bytes"

    lazy val humanBase10: String = human(10)
    lazy val humanBase2: String = human(2)

    private def human(base: Int) = {
        val units = StorageUnit.values.filter(_.base == base)

        var remainder = bytes
        val sb = new StringBuilder
        for (
            u <- units.sortBy(_.bytes).reverse
        ) {
            val dividend = remainder / u.bytes
            if (dividend > 0) {
                sb.append(s"+$dividend.${u.short}")
                remainder -= dividend * u.bytes
            }
        }
        sb.toString()
    }
}