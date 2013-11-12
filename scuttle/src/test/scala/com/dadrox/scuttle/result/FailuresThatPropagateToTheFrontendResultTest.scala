package com.dadrox.scuttle.result

import org.junit.Test
import org.fictus.Fictus
import com.dadrox.scuttle.Enum

object FailuresThatPropagateToTheFrontendResultTest {
    object HttpStatus extends Enum {
        sealed case class EnumVal private[HttpStatus] (name: String, status: Int) extends Value with Failure.Reason

        val NotFound = EnumVal("NotFound", 404)

        val InternalServerError = EnumVal("InternalServerError", 500)
        val BadGateway = EnumVal("BadGateway", 502)
        val GatewayTimeout = EnumVal("GatewayTimeout", 504)
    }

    // Define the types of failures you need.
    trait Failure {
        def description(): String
        def cause(): Option[Throwable]
    }
    object BackendFailure {
        object Reason extends Enum {
            sealed case class EnumVal private[Reason] (name: String) extends Value with Failure.Reason

            val JsonUnparseable = EnumVal("JsonUnparseable")
            val Timeout = EnumVal("Timeout")
        }
    }
    case class BackendFailure(reason: BackendFailure.Reason.EnumVal, message: String, cause: Option[Throwable] = None) extends Failure.Detail

    //    object FrontendFailure {
    //        implicit def backendFailure2frontend(backendFail: Failure[BackendFailure]): Failure[FrontendFailure] = {
    //            if (backend2Frontend.isDefinedAt(backendFail)) backend2Frontend.apply(backendFail)
    //            else backendFail match {
    //                case Failure(BackendFailure(BackendFailure.Reason.JsonUnparseable, desc, cause)) => FrontendFailure(HttpStatus.BadGateway, desc, cause)
    //            }
    //        }
    //
    //        def backend2Frontend(): PartialFunction[Failure[BackendFailure], Failure[FrontendFailure]] = {
    //            case Failure(BackendFailure(BackendFailure.Reason.Timeout, desc, cause)) => FrontendFailure(HttpStatus.GatewayTimeout, desc, cause)
    //        }
    //    }
    case class FrontendFailure(reason: HttpStatus.EnumVal, message: String, cause: Option[Throwable] = None) extends Failure.Detail

    implicit def backend2FrontendReason(): PartialFunction[BackendFailure.Reason.EnumVal, HttpStatus.EnumVal] = {
        case BackendFailure.Reason.Timeout => HttpStatus.GatewayTimeout
    }

    case class User()
    trait Backend {
        def fetchUser(): Result[Option[User]]
    }

    class FrontendService(backend: Backend) {
        def userByMatch(): Result[Option[User]] = backend.fetchUser match {
            case Success(Some(user))                              => Success(Some(user))
            case Success(None)                                    => Failure(FrontendFailure(HttpStatus.NotFound, "User not found :("))
            case f @ Failure(BackendFailure(reason, desc, cause)) => f
        }
    }
}

class FailuresThatPropagateToTheFrontendResultTest extends Fictus {
    import FailuresThatPropagateToTheFrontendResultTest._

    val user = User()

    val backend = mock[Backend]
    val unit = new FrontendService(backend)

    def fn = unit.userByMatch

    @Test
    def match_userFound {
        backend.fetchUser --> Success(Some(user))
        test(fn) mustEqual Success(Some(user))
    }

    @Test
    def match_userNotFound {
        backend.fetchUser --> Success(None)
        test(fn) mustMatch { case Failure(FrontendFailure(HttpStatus.NotFound, _, _)) => }
    }

    @Test
    def match_timeout {
        val failure = Failure(BackendFailure(BackendFailure.Reason.Timeout, "for test"))

        backend.fetchUser --> failure
        //        test(fn) mustMatch { case Failure(FrontendFailure(HttpStatus.GatewayTimeout, _, _)) => }
        test(fn) mustMatch { case `failure` => }
    }

    @Test
    def match_badJson {
        val failure = Failure(BackendFailure(BackendFailure.Reason.JsonUnparseable, "for test"))

        backend.fetchUser --> failure
        test(fn) mustMatch { case `failure` => }
    }
}

