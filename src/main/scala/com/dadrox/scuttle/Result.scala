package com.dadrox.scuttle

object Result {
    implicit def failure2Fail[F](failure: F): Fail[F] = Fail(failure)

    object converters {
        implicit def optionToResult[S, F](it: Option[S]) = new {
            def asResult[F1 >: F](fail: Fail[F1]): Result[S, F1] = it match {
                case Some(value) => Success(value)
                case None        => fail
            }
        }
    }
}

/** A response monad that carries detailed failure data.
 *  Sorta like a right-biased Either.
 */
sealed abstract class Result[+S, +F] { self =>
    def name(): String

    final def isSuccess(): Boolean = success.isDefined
    final def isFail(): Boolean = fail.isDefined

    final def success(): Option[S] = toOption

    final def fail(): Option[F] = this match {
        case Success(s) => None
        case Fail(f)    => Some(f)
    }

    final def toOption(): Option[S] = this match {
        case Success(s) => Some(s)
        case Fail(_)    => None
    }

    final def toSeq(): Seq[S] = this match {
        case Success(s) => Seq(s)
        case Fail(_)    => Seq.empty
    }

    final def exists(f: S => Boolean): Boolean = this match {
        case Success(s) => f(s)
        case Fail(_)    => false
    }

    final def forall(f: S => Boolean): Boolean = this match {
        case Success(s) => f(s)
        case Fail(_)    => false
    }

    // like map for the failure
    def handle[S1 >: S, F1](rescueFail: PartialFunction[F, F1]): Result[S1, F1] = this match {
        case Fail(f) if (rescueFail.isDefinedAt(f)) => Fail(rescueFail(f))
        //        case Fail(f)  => Fail(f)
        case Success(s)                             => Success(s)
    }

    // like flatMap for the failure
    def rescue[S1 >: S, F1](rescueFail: PartialFunction[F, Result[S1, F1]]): Result[S1, F1] = this match {
        case Fail(f) if (rescueFail.isDefinedAt(f)) => rescueFail(f)
        case Success(s)                             => Success(s)
    }

    def filter[F1 >: F](f: S => Boolean)(implicit ev: Fail.Convert[S] => F1): Result[S, F1] = this match {
        case Success(s) => if (f(s)) this else Fail(ev(Fail.Convert(s)))
        case Fail(f)    => Fail(f)
    }

    final def map[S1](f: S => S1): Result[S1, F] = this match {
        case Success(s) => Success(f(s))
        case Fail(f)    => Fail(f)
    }

    final def flatMap[S1, F1 >: F](f: S => Result[S1, F1]): Result[S1, F1] = this match {
        case Success(s) => f(s)
        case Fail(f)    => Fail(f)
    }

    def flatten[S1 >: S, F1 >: F, C](implicit evidence: S1 <:< Result[C, F1]): Result[C, F1] = this match {
        case Success(s) => s
        case Fail(f)    => Fail(f)
    }

    final def foreach[U](f: S => U) { toOption.foreach(f) }

    final def getOrElse[S1 >: S](default: => S1): S1 = toOption.getOrElse(default)

    final def orElse[S1 >: S, F1 >: F](default: => Result[S1, F1]): Result[S1, F1] = this match {
        case Success(s) => this
        case _          => default
    }

    final def onSuccess(f: S => Unit): Result[S, F] = {
        foreach(f)
        this
    }

    final def onFail(f: F => Unit): Result[S, F] = {
        fail.foreach(f)
        this
    }

    final def withFilter[F1 >: F](p: S => Boolean)(implicit ev: Fail.Convert[S] => F1): WithFilter[F1] = new WithFilter(p)

    class WithFilter[F1 >: F](p: S => Boolean)(implicit ev: Fail.Convert[S] => F1) {
        def map[S1](f: S => S1): Result[S1, F1] = self.filter[F1](p)(ev).map(f)
        def flatMap[S1](f: S => Result[S1, F1]): Result[S1, F1] = self.filter[F1](p)(ev).flatMap(f)
        def foreach[U](f: S => U): Unit = self.filter[F1](p)(ev).foreach(f)
        def withFilter(q: S => Boolean): WithFilter[F1] = new WithFilter(x => p(x) && q(x))
    }
}

object Success {
    case class Convert[+S](a: S)
}
final case class Success[+S](value: S) extends Result[S, Nothing] {
    override val name = "Success"
}

object Fail {
    case class Convert[+F](f: F)
}
final case class Fail[+F](failure: F) extends Result[Nothing, F] {
    override val name = "Fail"
}