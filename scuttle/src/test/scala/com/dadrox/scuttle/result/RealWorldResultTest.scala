package com.dadrox.scuttle.result

import org.junit.Test
import org.fictus.Fictus
import com.dadrox.scuttle.Enum

object RealWorldResultTest {
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
    case class FrontendFailure(reason: HttpStatus.EnumVal, message: String, cause: Option[Throwable] = None) extends Failure.Detail

    case class User()
    trait Backend {
        def fetchUser(): Result[Option[User]]
    }

    class FrontendService(backend: Backend) {

        // very verbose and cumbersome :/
        def userByMatch(): Result[User] = backend.fetchUser match {
            case Success(Some(user)) => Success(user)
            case Success(None)       => Failure(FrontendFailure(HttpStatus.NotFound, "User not found :("))
            case Failure(BackendFailure(reason, desc, cause)) => reason match {
                case BackendFailure.Reason.Timeout         => Failure(FrontendFailure(HttpStatus.GatewayTimeout, desc, cause))
                case BackendFailure.Reason.JsonUnparseable => Failure(FrontendFailure(HttpStatus.BadGateway, desc, cause))
            }
        }

        // a little better?
        def userByMatch2(): Result[User] = backend.fetchUser match {
            case Success(Some(user))                          => Success(user)
            case Success(None)                                => Failure(FrontendFailure(HttpStatus.NotFound, "User not found :("))
            case Failure(BackendFailure(reason, desc, cause)) => Failure(FrontendFailure(backendFailureReason2frontendReason(reason), "whoops", cause))
        }

        def backendFailureReason2frontendReason(reason: BackendFailure.Reason.EnumVal) = reason match {
            case BackendFailure.Reason.Timeout         => HttpStatus.GatewayTimeout
            case BackendFailure.Reason.JsonUnparseable => HttpStatus.BadGateway
        }
    }
}

class RealWorldResultTest extends Fictus {
    import RealWorldResultTest._

    val user = User()

    val backend = mock[Backend]
    val unit = new FrontendService(backend)

    def fn = unit.userByMatch

    @Test
    def match_userFound {
        backend.fetchUser --> Success(Some(user))
        test(fn) mustEqual Success(user)
    }

    @Test
    def match_userNotFound {
        backend.fetchUser --> Success(None)
        test(fn) mustMatch { case Failure(FrontendFailure(HttpStatus.NotFound, _, _)) => }
    }

    @Test
    def match_timeout {
        backend.fetchUser --> Failure(BackendFailure(BackendFailure.Reason.Timeout, "for test"))
        test(fn) mustMatch { case Failure(FrontendFailure(HttpStatus.GatewayTimeout, _, _)) => }
    }

    @Test
    def match_badJson {
        backend.fetchUser --> Failure(BackendFailure(BackendFailure.Reason.JsonUnparseable, "for test"))
        test(fn) mustMatch { case Failure(FrontendFailure(HttpStatus.BadGateway, _, _)) => }
    }
}

