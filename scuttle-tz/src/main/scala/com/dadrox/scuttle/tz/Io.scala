package com.dadrox.scuttle.tz

object Io {
    def directory(path: String) = {
        val dir = new java.io.File(path)
        if (!dir.isDirectory()) throw new IllegalArgumentException("The path provided must be a directory: " + path)
        else if (!dir.canRead()) throw new IllegalArgumentException("The directory provided must be readable: " + path)
        dir
    }

    def source(file: java.io.File) = io.Source.fromFile(file)(io.Codec("UTF-8"))
    def source(path: String) = io.Source.fromFile(path)(io.Codec("UTF-8"))
}