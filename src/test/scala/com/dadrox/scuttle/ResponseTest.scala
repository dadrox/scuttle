package com.dadrox.scuttle

import org.junit.Test
import org.fictus.Fictus

case class F(kind: Status.EnumVal, description: String, e: Option[Throwable] = None)

object Status extends Enum {
    sealed case class EnumVal private[Status] (name: String, status: Int) extends Value //with Failure

    val NotFound = EnumVal("NotFound", 404)
    val ServiceUnavailable = EnumVal("ServiceUnavailable", 503)
}

class ResponseTest extends Fictus {

    trait Io {
        def invoke()
    }

    val io = mock[Io]

    val f = F(Status.NotFound, "")
    val failed: Response[Int, F] = Fail[F](f)

    @Test
    def for_first_fail_is_the_result {
        val other: Response[Int, F] = Fail(F(Status.ServiceUnavailable, "2"))
        val result: Response[Int, F] = for {
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
    def toOption {
        Success(1).toOption mustEqual Some(1)
        failed.toOption mustEqual None
    }

    @Test
    def toSeq {
        Success(1).toSeq mustEqual Seq(1)
        failed.toSeq mustEqual Seq.empty
    }

    @Test
    def exists {
        Success(1).exists(1==) mustEqual true
        Success(1).exists(2==) mustEqual false
        failed.exists(1==) mustEqual false
    }

    @Test
    def forall {
        Success(1).forall(1==) mustEqual true
        Success(1).forall(2==) mustEqual false
        failed.forall(1==) mustEqual false
    }

    @Test
    def flatten {
        List(List(1)).flatten

        Success(Success(1)).flatten mustEqual Success(1)
        Success(failed).flatten mustEqual failed
    }

    @Test
    def flatMap {
        Success(1) flatMap (v => Success(v + 2)) mustEqual Success(3)

        failed flatMap (_ => Success(2)) mustEqual failed
    }

    @Test
    def map {
        Success(1) map (2+) mustEqual Success(3)

        failed map (2+) mustEqual failed
    }

    @Test
    def success {
        Success(1).success mustEqual Some(1)

        failed.success mustEqual None
    }

    @Test
    def failure {
        failed.fail mustEqual Some(f)

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
        }
    }

    @Test
    def withFilter_map {
        Success(1).withFilter(_ => true).map(1+) mustEqual Success(2)
        failed.withFilter(_ => true).map(1+) mustMatch { case Fail(_) => }
    }

    @Test
    def withFilter_flatMap {
        Success(1).withFilter(_ => true) flatMap (v => Success(v + 2)) mustEqual Success(3)
        failed.withFilter(_ => true) flatMap (_ => Success(2)) mustMatch { case Fail(_) => }
    }

    @Test
    def withFilter_withFilter {
        expect(io.invoke)

        test {
            Success(1).withFilter(_ => true).withFilter(_ => true).foreach(_ => io.invoke)
            failed.withFilter(_ => true).withFilter(_ => true).foreach(_ => io.invoke)
            Success(1).withFilter(_ => true).withFilter(_ => false).foreach(_ => io.invoke)
            failed.withFilter(_ => true).withFilter(_ => false).foreach(_ => io.invoke)
        }
    }

    @Test
    def getOrElse {
        Success(1) getOrElse 2 mustEqual 1

        failed getOrElse 2 mustEqual 2
    }

    @Test
    def orElse {
        Success(1) orElse Success(11) mustEqual Success(1)

        failed orElse Success(11) mustEqual Success(11)
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
            failed onFail (_ => io.invoke)
        }
        Success(2) onFail (_ => io.invoke)
    }
}