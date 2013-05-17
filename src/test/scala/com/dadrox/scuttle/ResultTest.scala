package com.dadrox.scuttle

import org.junit.Test
import org.fictus.Fictus

/*
 * To make dealing with the type a bit easier, define an implicit that convert the actual failure data case class to a Fail.
 * Also you can define a type that hides the Fail type completely for a given context.
 */
object Magic {
    implicit def fail2Fail(f: FailureData): Fail[FailureData] = Fail(FailureData(f.status, f.description, f.cause))

    type Response[A] = Result[A, FailureData]
}

object MatchFail {
    def unapply(f: Result[_, FailureData]) = f match {
        case Fail(FailureData(s, d, t)) => Some((s, d, t))
        case _                          => None
    }
}

case class FailureData(status: Int, description: String, cause: Option[Throwable] = None)

class ResultTest extends Fictus {
    import Magic._

    trait Io {
        def invoke()
    }

    val io = mock[Io]

    val failureData = FailureData(404, "")
    val rawFail = Fail(failureData)
    val failed: Result[Int, FailureData] = rawFail

    @Test
    def for_first_fail_is_the_result {
        val other: Response[Int] = FailureData(503, "2")
        val result = for {
            a <- failed
            b <- other
        } yield a + b
        result mustEqual failed
    }

    @Test
    def for_Fail_after_Success {
        val result = for {
            a <- Success(1)
            b <- failed
        } yield a + b
        result mustEqual failed
    }

    @Test
    def for_Fail_before_Success {
        val result = for {
            a <- failed
            b <- Success(a + 2)
        } yield a + b
        result mustEqual failed
    }

    @Test
    def for_no_Fails {
        val result = for {
            a <- Success(1)
            b <- Success(2)
        } yield a + b
        result mustEqual Success(3)
    }

    @Test
    def matching {
        Success(1) match {
            case Success(1) =>
            case _          => fail("Should have matched")
        }

        failed match {
            case Fail(FailureData(_, _, _)) =>
            case _                          => fail("Should have matched")
        }

        failed match {
            case xx @ MatchFail(s, d, t) =>
            case _                       => fail("Should have matched")
        }
    }

    @Test
    def toOption {
        Success(1).toOption mustEqual Some(1)
        failed.toOption mustEqual None
        rawFail.toOption mustEqual None
    }

    @Test
    def toSeq {
        Success(1).toSeq mustEqual Seq(1)
        failed.toSeq mustEqual Seq.empty
        rawFail.toSeq mustEqual Seq.empty
    }

    @Test
    def exists {
        Success(1).exists(1==) mustEqual true
        Success(1).exists(2==) mustEqual false
        failed.exists(1==) mustEqual false
        rawFail.exists(_ => false) mustEqual false
        rawFail.exists(_ => true) mustEqual false
    }

    @Test
    def forall {
        Success(1).forall(1==) mustEqual true
        Success(1).forall(2==) mustEqual false
        failed.forall(1==) mustEqual false
        rawFail.forall(_ => false) mustEqual false
        rawFail.forall(_ => true) mustEqual false
    }

    @Test
    def flatten {
        Success(Success(Success(1))).flatten mustEqual Success(Success(1))
        Success(Success(1)).flatten mustEqual Success(1)
        Success(failed).flatten mustEqual failed

        // Doesn't compile
        // Success(rawFail).flatten mustEqual failed
        // failed.flatten mustEqual failed
    }

    @Test
    def rescue_Success {
        Success(1).rescue {
            case `failed` => 2
        } mustEqual Success(1)
    }

    @Test
    def rescue_Fail {
        failed.rescue {
            case `failureData` => 2
        } mustEqual Success(2)

        failed.rescue {
            case `failureData` => "win"
        } mustEqual Success("win")

        failed.rescue {
            case FailureData(-1, "won't match", _) => 2
        } mustEqual failed
    }

    @Test
    def rescueFlat_Success {
        Success(1).rescueFlat {
            case `failed` => Success(2)
        } mustEqual Success(1)
    }

    @Test
    def rescueFlat_Fail {
        failed.rescueFlat {
            case `failureData` => Success(2)
        } mustEqual Success(2)

        failed.rescueFlat {
            case FailureData(-1, "won't match", _) => Success(2)
        } mustEqual failed
    }

    @Test
    def flatMap {
        Success(1) flatMap (v => Success(v + 2)) mustEqual Success(3)

        failed flatMap (_ => Success(2)) mustEqual failed
        rawFail flatMap (_ => Success(2)) mustEqual failed
    }

    @Test
    def map {
        Success(1) map (2+) mustEqual Success(3)

        failed map (2+) mustEqual failed
        rawFail map (_ => 1) mustEqual failed
    }

    @Test
    def success {
        Success(1).success mustEqual Some(1)

        failed.success mustEqual None
        rawFail.success mustEqual None
    }

    @Test
    def failure {
        failed.fail mustEqual Some(failureData)
        rawFail.fail mustEqual Some(failureData)

        Success(1).fail mustEqual None
    }

    // TODO How can I get rid of this?
    implicit def i2f(xx: Fail.Convert[Int]) = {
        new RuntimeException().fillInStackTrace().printStackTrace()
        null
    }

    @Test
    def filter {
        Success(1) filter (1==) mustEqual Success(1)
        Success(1) filter (2==) mustMatch { case Fail(_) => }
        failed filter (2==) mustMatch { case Fail(_) => }
        rawFail filter (_ => false) mustMatch { case Fail(_) => }
        rawFail filter (_ => true) mustMatch { case Fail(_) => }
    }

    @Test
    def withFilter_foreach {
        expect(io.invoke) times 3

        test {
            Success(1).withFilter(_ => true).foreach(_ => io.invoke)
            failed.withFilter(_ => true).foreach(_ => io.invoke)
            Success(1).withFilter(_ => true).foreach(_ => io.invoke)
            failed.withFilter(_ => true).foreach(_ => io.invoke)
            Success(1).withFilter(_ => true).foreach(_ => io.invoke)
            failed.withFilter(_ => true).foreach(_ => io.invoke)
            rawFail.withFilter(_ => true).foreach(_ => io.invoke)
        }
    }

    @Test
    def withFilter_map {
        Success(1).withFilter(_ => true).map(1+) mustEqual Success(2)
        failed.withFilter(_ => true).map(1+) mustMatch { case Fail(_) => }
        rawFail.withFilter(_ => true).map(_ => 1) mustMatch { case Fail(_) => }
    }

    @Test
    def withFilter_flatMap {
        Success(1).withFilter(_ => true) flatMap (v => Success(v + 2)) mustEqual Success(3)
        failed.withFilter(_ => true) flatMap (_ => Success(2)) mustMatch { case Fail(_) => }
        rawFail.withFilter(_ => true) flatMap (_ => Success(2)) mustMatch { case Fail(_) => }
    }

    @Test
    def withFilter_withFilter {
        expect(io.invoke)

        test {
            Success(1).withFilter(_ => true).withFilter(_ => true).foreach(_ => io.invoke)
            failed.withFilter(_ => true).withFilter(_ => true).foreach(_ => io.invoke)
            Success(1).withFilter(_ => true).withFilter(_ => false).foreach(_ => io.invoke)
            failed.withFilter(_ => true).withFilter(_ => false).foreach(_ => io.invoke)
            rawFail.withFilter(_ => true).withFilter(_ => false).foreach(_ => io.invoke)
        }
    }

    @Test
    def getOrElse {
        Success(1) getOrElse 2 mustEqual 1

        failed getOrElse 2 mustEqual 2
        rawFail getOrElse 2 mustEqual 2
    }

    @Test
    def orElse {
        Success(1) orElse Success(11) mustEqual Success(1)

        failed orElse Success(11) mustEqual Success(11)
        rawFail orElse Success(11) mustEqual Success(11)
    }

    @Test
    def foreach {
        expect(io.invoke) times 3

        test {
            Success(1) foreach (_ => io.invoke)
            failed foreach (_ => io.invoke)
            Success(2) foreach (_ => io.invoke)
            failed foreach (_ => io.invoke)
            Success(3) foreach (_ => io.invoke)
            rawFail foreach (_ => io.invoke)
        }
    }

    @Test
    def onSuccess {
        expect(io.invoke) times 3

        test {
            Success(1) onSuccess (_ => io.invoke)
            failed onSuccess (_ => io.invoke)
            Success(2) onSuccess (_ => io.invoke)
            failed onSuccess (_ => io.invoke)
            Success(3) onSuccess (_ => io.invoke)
            rawFail onSuccess (_ => io.invoke)
        }
        failed onSuccess (_ => io.invoke)
    }

    @Test
    def onFail {
        expect(io.invoke) times 3

        test {
            failed onFail (_ => io.invoke)
            Success(1) onFail (_ => io.invoke)
            failed onFail (_ => io.invoke)
            Success(2) onFail (_ => io.invoke)
            rawFail onFail (_ => io.invoke)
        }
        Success(1) onFail (_ => io.invoke)
    }
}