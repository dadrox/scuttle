package com.dadrox.scuttle.result

// TODO:
//  - Some notion of "tracing" that shows how results are chained? Sorta like stacktraces

/** A response monad that carries detailed failure data.
 *  Sorta like a right-biased Either.
 */
sealed abstract class Result[+S] {
    def name(): String

    final def isSuccess(): Boolean = success.isDefined
    final def isFailure(): Boolean = failure.isDefined

    final def success(): Option[S] = toOption

    final def failure(): Option[Failure] = this match {
        case Success(s) => None
        case f: Failure => Some(f)
    }

    final def toOption(): Option[S] = this match {
        case Success(s) => Some(s)
        case _          => None
    }

    final def toSeq(): Seq[S] = this match {
        case Success(s) => Seq(s)
        case _          => Seq.empty
    }

    final def exists(f: S => Boolean): Boolean = this match {
        case Success(s) => f(s)
        case _          => false
    }

    final def forall(f: S => Boolean): Boolean = this match {
        case Success(s) => f(s)
        case _          => false
    }

    def rescue[S1 >: S](rescueFail: PartialFunction[Failure, S1]): Result[S1] = this match {
        case f: Failure if (rescueFail.isDefinedAt(f)) => Success(rescueFail(f))
        case _                                         => this
    }

    def rescueFlat[S1 >: S](rescueFail: PartialFunction[Failure, Result[S1]]): Result[S1] = this match {
        case f: Failure if (rescueFail.isDefinedAt(f)) => rescueFail(f)
        case _                                         => this
    }

    final def map[S1](f: S => S1): Result[S1] = this match {
        case Success(s) => Success(f(s))
        case f: Failure => f
    }

    final def flatMap[S1](f: S => Result[S1]): Result[S1] = this match {
        case Success(s) => f(s)
        case f: Failure => f
    }

    final def flatten[S1 >: S, C](implicit evidence: S1 <:< Result[C]): Result[C] = this match {
        case Success(s) => s
        case f: Failure => f
    }

    final def foreach[U](f: S => U) { toOption.foreach(f) }

    final def getOrElse[S1 >: S](default: => S1): S1 = toOption.getOrElse(default)

    final def orElse[S1 >: S](default: => Result[S1]): Result[S1] = this match {
        case Success(s) => this
        case _          => default
    }

    final def onSuccess(f: S => Unit): Result[S] = {
        foreach(f)
        this
    }

    final def onFail(f: Failure => Unit): Result[S] = {
        failure.foreach(f)
        this
    }

    //    def filter(f: S => Boolean)(implicit ev: Failure.Convert => Failure): Result[S] = this match {
    //        case Success(s) => if (f(s)) this else Failure(ev(Failure.Convert(s)))
    //        case f: Failure => Failure(f)
    //    }
    //
    //    final def withFilter(p: S => Boolean)(implicit ev: Failure.Convert[S] => F1): WithFilter[F1] = new WithFilter(p)
    //
    //    class WithFilter(p: S => Boolean)(implicit ev: Failure.Convert[S] => F1) {
    //        def map[S1](f: S => S1): Result[S1] = self.filter[F1](p)(ev).map(f)
    //        def flatMap[S1](f: S => Result[S1]): Result[S1] = self.filter[F1](p)(ev).flatMap(f)
    //        def foreach[U](f: S => U): Unit = self.filter[F1](p)(ev).foreach(f)
    //        def withFilter(q: S => Boolean): WithFilter[F1] = new WithFilter(x => p(x) && q(x))
    //    }
}

object Success {
    case class Convert[+S](a: S)
}
final case class Success[+S](value: S) extends Result[S] {
    override val name = "Success"
}

object Failure {

    trait Reason {
        def name(): String
    }

    trait Detail {
        def reason(): Reason
        def message(): String
        def cause(): Option[Throwable]
    }

    case class Convert(f: Failure)
}
final case class Failure(detail: Failure.Detail) extends Result[Nothing] {
    override val name = "Failure"
}