package com.dadrox.scuttle.result

import org.fictus.Fictus

object MatchFail {
    def unapply(f: Result[_]) = f match {
        case Failure(s, d, t) => Some((s, d, t))
        case _                => None
    }
}

private[result] case class Status(code: Int) extends Failure.Reason

class ResultTest extends Fictus {

    trait Io {
        def invoke()
    }

    val io = mock[Io]

    val rawFail = Failure(Status(404), "")
    val failed: Result[Int] = rawFail

    @Test
    def for_first_fail_is_the_result {
        val other: Result[Int] = Failure(Status(503), "2")
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
            case Failure(_, _, _) =>
            case _                => fail("Should have matched")
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
        Success(rawFail).flatten mustEqual rawFail

        // Doesn't compile
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
            case `rawFail` => 2
        } mustEqual Success(2)

        failed.rescue {
            case `rawFail` => "win"
        } mustEqual Success("win")

        failed.rescue {
            case Failure(Status(-1), "won't match", _) => 2
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
            case `rawFail` => Success(2)
        } mustEqual Success(2)

        failed.rescueFlat {
            case Failure(Status(-1), "won't match", _) => Success(2)
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
        failed.failure mustEqual Some(rawFail)
        rawFail.failure mustEqual Some(rawFail)

        Success(1).failure mustEqual None
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
    def onFailure {
        expect(io.invoke) times 3

        test {
            failed onFailure (_ => io.invoke)
            Success(1) onFailure (_ => io.invoke)
            failed onFailure (_ => io.invoke)
            Success(2) onFailure (_ => io.invoke)
            rawFail onFailure (_ => io.invoke)
        }
        Success(1) onFailure (_ => io.invoke)
    }

    @Test
    def result_of_void {
        val xx: Result[Void] = Success(Void)

        xx.success() mustEqual Some(Void)
    }

    implicit def convertToFailure(i: Int) = rawFail

    @Test
    def filter {
        Success(1) filter (1==) mustEqual Success(1)
        Success(1) filter (2==) mustMatch {
            case Failure(Failure.FilterPredicateFalse, _, _) =>
        }
        failed filter (2==) mustEqual failed
        rawFail filter (_ => false) mustEqual rawFail
        rawFail filter (_ => true) mustEqual rawFail
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
        failed.withFilter(_ => true).map(1+) mustMatch { case Failure(_, _, _) => }
        rawFail.withFilter(_ => true).map(_ => 1) mustMatch { case Failure(_, _, _) => }
    }

    @Test
    def withFilter_flatMap {
        Success(1).withFilter(_ => true) flatMap (v => Success(v + 2)) mustEqual Success(3)
        failed.withFilter(_ => true) flatMap (_ => Success(2)) mustMatch { case Failure(_, _, _) => }
        rawFail.withFilter(_ => true) flatMap (_ => Success(2)) mustMatch { case Failure(_, _, _) => }
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
    def callInfo {
        rawFail.toString mustContain ("ResultTest")
    }
}