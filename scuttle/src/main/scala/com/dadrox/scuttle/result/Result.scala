package com.dadrox.scuttle.result

import com.dadrox.scuttle.CallInfo
import com.dadrox.scuttle.time.Duration

object Result {
    /** If the Results are all Successes, converts a seq of Result into a Result of seq.
     *  Otherwise, converts to the first Failure.
     */
    // TODO? accumulate the Failure.Detail messages into one?
    def collect[A](rs: Seq[Result[A]]): Result[Seq[A]] = rs match {
        case Nil => Success(Nil)
        case results => results.flatMap(_.failure()) match {
            case failure +: rest => failure
            case Seq()           => Success(results.flatMap(_.success()))
        }
    }
}

/** A response monad that carries detailed failure data.
 *  Sorta like a right-biased Either.
 */
sealed abstract class Result[+S] { self =>
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

    def filter(f: S => Boolean): Result[S] = this match {
        case Success(s) => if (f(s)) this else Failure(Failure.FilterPredicateFalse, s"Filter failed on $s")
        case failure    => failure
    }

    final def withFilter(p: S => Boolean): WithFilter = new WithFilter(p)

    class WithFilter(p: S => Boolean) {
        def map[S1](f: S => S1): Result[S1] = self.filter(p).map(f)
        def flatMap[S1](f: S => Result[S1]): Result[S1] = self.filter(p).flatMap(f)
        def foreach[U](f: S => U): Unit = self.filter(p).foreach(f)
        def withFilter(q: S => Boolean): WithFilter = new WithFilter(x => p(x) && q(x))
    }
}

final case class Success[+S](value: S) extends Result[S] {
    override val name = "Success"
}

object Failure {
    trait Reason
    trait Timeout extends Reason
    case object NoReason extends Reason

    case object FilterPredicateFalse extends Reason
}

final case class Failure(reason: Failure.Reason, message: String, cause: Option[Throwable] = None)(implicit val callInfo: CallInfo = CallInfo.callSite) extends Result[Nothing] {
    override val name = "Failure"
    override lazy val toString = s"Failure($reason, $message, $cause): $callInfo"
}
