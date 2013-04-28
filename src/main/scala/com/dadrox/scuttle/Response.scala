package com.dadrox.scuttle

trait Failure {
    def name: String
}
trait FailureData {
    def kind(): Failure
    def description(): String
    def e(): Option[Throwable]
}

object Response {

    implicit def failureData2Fail(failure: FailureData) = Fail(failure)

    def apply(failure: FailureData) = Fail(failure)
    def apply[A](a: A): Response[A] = a match {
        case failure: FailureData => Fail(failure)
        case success              => Success(success)
    }

    case class PredicateMissing() extends FailureData {
        val kind = new Failure { val name = "PredicateMissing" }
        val description = "Not matched"
        val e = None
    }
}

/** A response monad that carries detailed failure data.
 */
sealed abstract class Response[+A] {
    def name(): String
    def isSuccess(): Boolean
    def isFail(): Boolean
    def success(): Option[A]
    def fail(): Option[FailureData]

    //    // like map for the failure
    //    def handle
    //    // liek flatMap for the failue
    //    def rescue

    final def filter(f: A => Boolean): Response[A] = this match {
        case Success(s) if (f(s)) => this
        case _                    => Fail(Response.PredicateMissing())
    }

    final def map[B](f: A => B): Response[B] = this match {
        case Success(s) => Success(f(s))
        case Fail(f)    => Fail(f)
    }

    final def flatMap[B](f: A => Response[B]): Response[B] = this match {
        case Success(s) => f(s)
        case Fail(f)    => Fail(f)
    }

    final def flatten[B](implicit evidence: A <:< Response[B]): Response[B] = this match {
        case Success(s) => s
        case Fail(f)    => Fail(f)
    }

    final def foreach(f: A => Unit) { success.foreach(f) }

    final def getOrElse[B >: A](default: => B): B = success.getOrElse(default)

    final def orElse[B >: A](default: => Response[B]): Response[B] = this match {
        case Success(s) => this
        case _          => default
    }

    final def onSuccess(f: A => Unit): Response[A] = {
        foreach(f)
        this
    }

    final def onFail(f: FailureData => Unit): Response[A] = {
        fail.foreach(f)
        this
    }
}

final case class Success[+A](value: A) extends Response[A] {
    override val name = "Success"
    override val isSuccess = true
    override val isFail = false

    def success() = Some(value)
    def fail() = None
}

final case class Fail[+A, F](failure: FailureData) extends Response[A] {
    override val name = "Fail"
    override val isSuccess = false
    override val isFail = true

    def success() = None
    def fail() = Some(failure)
}