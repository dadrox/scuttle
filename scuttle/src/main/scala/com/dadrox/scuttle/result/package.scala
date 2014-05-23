package com.dadrox.scuttle

package object result {

    implicit class OptionToResult[S](it: Option[S]) {
        def failOnNone(failure: Failure): Result[S] = it match {
            case Some(value) => Success(value)
            case _           => failure
        }
    }

    type ExecutionContext = scala.concurrent.ExecutionContext
    val ExecutionContext = scala.concurrent.ExecutionContext
}