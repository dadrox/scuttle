package com.dadrox.scuttle

package object predef {
    def prln[A](a: A) = {
        println(a)
        a
    }

    def prln() = println()

    def pr[A](a: A) = {
        print(a)
        a
    }
}