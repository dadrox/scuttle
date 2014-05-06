package com.dadrox.scuttle

package object result {

    implicit class OptionToResult[S](val it: Option[S]) extends AnyVal {
        def failOnNone(failure: Failure): Result[S] = it match {
            case Some(value) => Success(value)
            case _           => failure
        }
    }

    type ExecutionContext = scala.concurrent.ExecutionContext
    val ExecutionContext = scala.concurrent.ExecutionContext
}