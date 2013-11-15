package com.dadrox.scuttle.result

object converters {
    implicit class OptionToResult[S](it: Option[S]) {
        def asResult(failure: Failure): Result[S] = it match {
            case Some(value) => Success(value)
            case None        => failure
        }
    }
}