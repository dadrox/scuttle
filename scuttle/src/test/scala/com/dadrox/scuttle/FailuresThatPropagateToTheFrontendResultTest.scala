package com.dadrox.scuttle

import org.junit.Test
import org.fictus.Fictus

object FailuresThatPropagateToTheFrontendResultTest {
    object HttpStatus extends Enum {
        sealed case class EnumVal private[HttpStatus] (name: String, status: Int) extends Value

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
            sealed case class EnumVal private[Reason] (name: String) extends Value

            val JsonUnparseable = EnumVal("JsonUnparseable")
            val Timeout = EnumVal("Timeout")
        }
    }
    case class BackendFailure(reason: BackendFailure.Reason.EnumVal, description: String, cause: Option[Throwable] = None) extends Failure

    object FrontendFailure {
        implicit def backendFailure2frontend(backendFail: Fail[BackendFailure]): Fail[FrontendFailure] = {
            if (backend2Frontend.isDefinedAt(backendFail)) backend2Frontend.apply(backendFail)
            else backendFail match {
                case Fail(BackendFailure(BackendFailure.Reason.JsonUnparseable, desc, cause)) => FrontendFailure(HttpStatus.BadGateway, desc, cause)
            }
        }

        def backend2Frontend(): PartialFunction[Fail[BackendFailure], Fail[FrontendFailure]] = {
            case Fail(BackendFailure(BackendFailure.Reason.Timeout, desc, cause)) => FrontendFailure(HttpStatus.GatewayTimeout, desc, cause)
        }
    }
    case class FrontendFailure(status: HttpStatus.EnumVal, description: String, cause: Option[Throwable] = None) extends Failure

    implicit def backend2FrontendReason(): PartialFunction[BackendFailure.Reason.EnumVal, HttpStatus.EnumVal] = {
        case BackendFailure.Reason.Timeout => HttpStatus.GatewayTimeout
    }

    case class User()
    trait Backend {
        def fetchUser(): Result[Option[User], BackendFailure]
    }

    class FrontendService(backend: Backend) {
        def userByMatch(): Result[Option[User], FrontendFailure] = backend.fetchUser match {
            case Success(Some(user))                           => Success(Some(user))
            case Success(None)                                 => FrontendFailure(HttpStatus.NotFound, "User not found :(")
            case f @ Fail(BackendFailure(reason, desc, cause)) => f
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
        test(fn) mustMatch { case Fail(FrontendFailure(HttpStatus.NotFound, _, _)) => }
    }

    @Test
    def match_timeout {
        backend.fetchUser --> Fail(BackendFailure(BackendFailure.Reason.Timeout, "for test"))
        test(fn) mustMatch { case Fail(FrontendFailure(HttpStatus.GatewayTimeout, _, _)) => }
    }

    @Test
    def match_badJson {
        backend.fetchUser --> Fail(BackendFailure(BackendFailure.Reason.JsonUnparseable, "for test"))
        test(fn) mustMatch { case Fail(FrontendFailure(HttpStatus.BadGateway, _, _)) => }
    }
}

