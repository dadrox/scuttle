package com.dadrox.scuttle

import org.junit.Test
import org.fictus.Fictus

object CommonFailuresResultTest {
    object HttpStatus extends Enum {
        sealed case class EnumVal private[HttpStatus] (name: String, status: Int) extends Value

        val NotFound = EnumVal("NotFound", 404)

        val InternalServerError = EnumVal("InternalServerError", 500)
        val BadGateway = EnumVal("BadGateway", 502)
        val GatewayTimeout = EnumVal("GatewayTimeout", 504)
    }

    // A common failure to be used throughout the system.
    case class Failure(status: HttpStatus.EnumVal, description: String, cause: Option[Throwable] = None)

    // Since all failures are the same, we define a convenience type
    type Response[A] = Result[A, Failure]

    case class User()
    trait Backend {
        def fetchUser(): Result[Option[User], Failure]
    }

    class FrontendService(backend: Backend) {
        def userByMatch(): Result[User, Failure] = backend.fetchUser match {
            case Success(Some(user)) => Success(user)
            case Success(None)       => Fail(Failure(HttpStatus.NotFound, "User not found :("))
            case Fail(f)             => Fail(f)
        }

        def userByFlatMap(): Result[User, Failure] = {
            backend.fetchUser.flatMap {
                case Some(user) => Success(user)
                case None       => Fail(Failure(HttpStatus.NotFound, "User not found :("))
            }
        }

        def userByFlatMap2(): Result[User, Failure] = {
            import com.dadrox.scuttle.Result.converters._
            backend.fetchUser.flatMap { _.asResult(Fail(Failure(HttpStatus.NotFound, "User not found :("))) }
        }

        def userByForComprehension(): Result[User, Failure] = {
            import com.dadrox.scuttle.Result.converters._
            for {
                userOpt <- backend.fetchUser
                user <- userOpt.asResult(Fail(Failure(HttpStatus.NotFound, "User not found :(")))
            } yield user
        }
    }
}

class CommonFailuresResultTest extends Fictus {
    import CommonFailuresResultTest._

    val user = User()

    val backend = mock[Backend]
    val unit = new FrontendService(backend)

    //    def fn = unit.userByMatch
    //    def fn = unit.userByFlatMap
    //    def fn = unit.userByFlatMap2
    def fn = unit.userByForComprehension

    @Test
    def match_userFound {
        backend.fetchUser --> Success(Some(user))
        test(fn) mustEqual Success(user)
    }

    @Test
    def match_userNotFound {
        backend.fetchUser --> Success(None)
        test(fn) mustMatch { case Fail(Failure(HttpStatus.NotFound, _, _)) => }
    }

    @Test
    def match_timeout {
        backend.fetchUser --> Fail(Failure(HttpStatus.GatewayTimeout, "for test"))
        test(fn) mustMatch { case Fail(Failure(HttpStatus.GatewayTimeout, _, _)) => }
    }

    @Test
    def match_badJson {
        backend.fetchUser --> Fail(Failure(HttpStatus.BadGateway, "for test"))
        test(fn) mustMatch { case Fail(Failure(HttpStatus.BadGateway, _, _)) => }
    }
}

