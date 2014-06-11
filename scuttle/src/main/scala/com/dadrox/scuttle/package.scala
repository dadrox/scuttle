package com.dadrox

package object scuttle {

    implicit class HumanNumber(i: Int) {
        def thousand = i * 1000
        def K = thousand

        def million = i.K * 1000
        def M = million

        def billion = i.M * 1000
        def B = billion
    }

    implicit class HumanNumberLong(i: Long) {
        def thousand = i * 1000
        def K = thousand

        def million = i.K * 1000
        def M = million

        def billion = i.M * 1000
        def B = billion
    }
}