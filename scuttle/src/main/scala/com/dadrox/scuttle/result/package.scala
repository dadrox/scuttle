package com.dadrox.scuttle

package object result {

    implicit class PimpedResultOption[S](it: Result[Option[S]]) {
    }

    implicit class PimpedResult[S](it: Result[S]) {
        def asFuture = resultToFuture(it)
    }

    implicit def resultToFuture[S](it: Result[S]): Future[S] = it match {
        case Success(s) => Future.success(s)
        case f: Failure => Future.fail(f)
    }
    implicit def successToFuture[S](it: Success[S]): Future[S] = it match {
        case Success(s) => Future.success(s)
    }
    implicit def failureToFuture[S](it: Failure): Future[S] = it match {
        case f: Failure => Future.fail(f)
    }

    implicit class OptionToResult[S](it: Option[S]) {
        def asResult(failure: Failure): Result[S] = it match {
            case Some(value) => Success(value)
            case None        => failure
        }
    }

    type ExecutionContext = scala.concurrent.ExecutionContext
    val ExecutionContext = scala.concurrent.ExecutionContext
}