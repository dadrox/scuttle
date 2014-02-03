package com.dadrox.scuttle

package object result {
    implicit class OptionToResult[S](it: Option[S]) {
        def asResult(none: Failure.Detail): Result[S] = it match {
            case Some(value) => Success(value)
            case None        => Failure(none)
        }
    }

    type ExecutionContext = scala.concurrent.ExecutionContext
    val ExecutionContext = scala.concurrent.ExecutionContext
}