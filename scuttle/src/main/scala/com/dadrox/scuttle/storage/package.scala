package com.dadrox.scuttle

import com.dadrox.scuttle.storage.Storage

package object storage {
    implicit def IntToStorage(value: Int) = new LongToStorage(value)

    implicit class LongToStorage(value: Long) {

        def bytes = Storage.fromBytes(value)
        def byte = bytes
        def B = bytes

        def kilobytes = Storage.fromKilobytes(value)
        def kilobyte = kilobytes
        def kB = kilobytes
        def KB = kilobytes
        def megabytes = Storage.fromMegabytes(value)
        def megabyte = megabytes
        def MB = megabytes
        def gigabytes = Storage.fromGigabytes(value)
        def gigabyte = gigabytes
        def GB = gigabytes
        def terabytes = Storage.fromTerabytes(value)
        def terabyte = terabytes
        def TB = terabytes
        def petabytes = Storage.fromPetabytes(value)
        def petabyte = petabytes
        def PB = petabytes
        def exabytes = Storage.fromExabytes(value)
        def exabyte = exabytes
        def EB = exabytes

        def kibibytes = Storage.fromKibibytes(value)
        def kibibyte = kibibytes
        def KiB = kibibytes
        def mebibytes = Storage.fromMebibytes(value)
        def mebibyte = mebibytes
        def MiB = mebibytes
        def gibibytes = Storage.fromGibibytes(value)
        def gibibyte = gibibytes
        def GiB = gibibytes
        def tebibytes = Storage.fromTebibytes(value)
        def tebibyte = tebibytes
        def TiB = tebibytes
        def pebibytes = Storage.fromPebibytes(value)
        def pebibyte = pebibytes
        def PiB = pebibytes
        def exbibytes = Storage.fromExbibytes(value)
        def exbibyte = exbibytes
        def EiB = exbibytes
    }
}