package com.dadrox.scuttle

object Response {
}

/** A response monad that carries detailed failure data.
 *  Sorta like a right-biased Either.
 */
sealed abstract class Response[+A, +F] { self =>
    def name(): String

    final def isSuccess(): Boolean = success.isDefined
    final def isFail(): Boolean = fail.isDefined

    final def success(): Option[A] = toOption

    final def fail(): Option[F] = this match {
        case Success(s) => None
        case Fail(f)    => Some(f)
    }

    final def toOption() = this match {
        case Success(s) => Some(s)
        case Fail(_)    => None
    }

    final def toSeq() = this match {
        case Success(s) => Seq(s)
        case Fail(_)    => Seq.empty
    }

    final def exists(f: A => Boolean) = this match {
        case Success(s) => f(s)
        case Fail(_)    => false
    }

    final def forall(f: A => Boolean) = this match {
        case Success(s) => f(s)
        case Fail(_)    => false
    }

    //    // like map for the failure
    //    def handle
    //    // like flatMap for the failue
    //    def rescue

    def filter[F1 >: F](f: A => Boolean)(implicit ev: Fail.Convert[A] => F1): Response[A, F1] = this match {
        case Success(s) => if (f(s)) this else Fail(ev(Fail.Convert(s)))
        case Fail(f)    => Fail(f)
    }

    final def map[B](f: A => B): Response[B, F] = this match {
        case Success(s) => Success(f(s))
        case Fail(f)    => Fail(f)
    }

    final def flatMap[B >: A, G >: F](f: A => Response[B, G]): Response[B, G] = this match {
        case Success(s) => f(s)
        case Fail(f)    => Fail(f)
    }

    def flatten[B >: A, G >: F, C](implicit evidence: B <:< Response[C, G]): Response[C, G] = this match {
        case Success(s) => s
        case Fail(f)    => Fail(f)
    }

    final def foreach[U](f: A => U) { toOption.foreach(f) }

    final def getOrElse[B >: A](default: => B): B = toOption.getOrElse(default)

    final def orElse[B >: A, G >: F](default: => Response[B, G]): Response[B, G] = this match {
        case Success(s) => this
        case _          => default
    }

    final def onSuccess(f: A => Unit): Response[A, F] = {
        foreach(f)
        this
    }

    final def onFail(f: F => Unit): Response[A, F] = {
        fail.foreach(f)
        this
    }

    final def withFilter[F1 >: F](p: A => Boolean)(implicit ev: Fail.Convert[A] => F1): WithFilter[F1] = new WithFilter(p)

    class WithFilter[F1 >: F](p: A => Boolean)(implicit ev: Fail.Convert[A] => F1) {
        def map[B](f: A => B): Response[B, F1] = self.filter[F1](p)(ev).map(f)
        def flatMap[B >: A, G >: F1](f: A => Response[B, G]): Response[B, G] = self.filter[F1](p)(ev).flatMap(f)
        def foreach[U](f: A => U): Unit = self.filter[F1](p)(ev).foreach(f)
        def withFilter(q: A => Boolean): WithFilter[F1] = new WithFilter(x => p(x) && q(x))
    }
}

object Success {
    case class Convert[+A](a: A)
}
final case class Success[+A](value: A) extends Response[A, Nothing] {
    override val name = "Success"
}

object Fail {
    case class Convert[+F](f: F)
}
final case class Fail[+F](failure: F) extends Response[Nothing, F] {
    override val name = "Fail"
}