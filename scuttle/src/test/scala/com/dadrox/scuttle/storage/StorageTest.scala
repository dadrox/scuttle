package com.dadrox.scuttle.storage

import com.dadrox.scuttle.storage._

class StorageTest extends org.fictus.Fictus {
    @Test
    def base10_creation_works {
        3.bytes mustEqual Storage.fromBytes(3)
        3.kilobytes mustEqual Storage.fromKilobytes(3)
        3.megabytes mustEqual Storage.fromMegabytes(3)
        3.gigabytes mustEqual Storage.fromGigabytes(3)
        3.terabytes mustEqual Storage.fromTerabytes(3)
        3.petabytes mustEqual Storage.fromPetabytes(3)
        3.exabytes mustEqual Storage.fromExabytes(3)
    }

    @Test
    def base10_conversions {
        3.megabytes.inBytes mustEqual 3 * 1000L * 1000
        3.mebibytes.inBytes mustEqual 3 * 1024L * 1024
    }

    @Test
    def base2_creation_works {
        3.bytes mustEqual Storage.fromBytes(3)
        3.kibibytes mustEqual Storage.fromKibibytes(3)
        3.mebibytes mustEqual Storage.fromMebibytes(3)
        3.gibibytes mustEqual Storage.fromGibibytes(3)
        3.tebibytes mustEqual Storage.fromTebibytes(3)
        3.pebibytes mustEqual Storage.fromPebibytes(3)
        3.exbibytes mustEqual Storage.fromExbibytes(3)
    }

    @Test
    def operations {
        (3.MB + 2.GB).inBytes mustEqual 3 * 1000L * 1000 + 2 * 1000L * 1000 * 1000
        (2.GB - 3.MB).inBytes mustEqual 2 * 1000L * 1000 * 1000 - 3 * 1000L * 1000
        3.MB + 2.MB mustEqual 5.MB
        3.EB / 3 mustEqual 1.EB
        2.PB * 10 mustEqual 20.PB
        6.GB * 0.5 mustEqual 3.GB
        6.GB / 0.5 mustEqual 12.GB
    }

    @Test
    def floor {
        (3.MB + 2.GB) floor 1.GB mustEqual 2.GB
        (2.GB + 3.MB + 5.kB + 10.bytes) floor 1.kB mustEqual (2.GB + 3.MB + 5.kB)
    }

    @Test
    def ceiling {
        (3.MB + 2.GB) ceiling 1.GB mustEqual 3.GB
        (2.GB + 3.MB + 5.kB + 10.bytes) ceiling 1.kB mustEqual (2.GB + 3.MB + 6.kB)
    }

    @Test
    def floor_stays_where_it_is_if_even_already {
        5.PB floor 1.PB mustEqual 5.PB
        1.megabyte floor 1.kB mustEqual 1.MB
    }

    @Test
    def ceiling_stays_where_it_is_if_even_already {
        5.PB ceiling 1.PB mustEqual 5.PB
        1.megabyte ceiling 1.kB mustEqual 1.MB
    }

    @Test
    def toString_is_decent {
        val it = 3.megabytes + 2.kilobytes + 100.bytes
        it.toString mustEqual "3002100.bytes"
        it.humanBase10 mustEqual "+3.MB+2.kB+100.B"
        it.humanBase2 mustEqual "+2.MiB+883.KiB+756.B"
    }
}