package com.dadrox.scuttle.result

import org.fictus.Fictus
import com.dadrox.scuttle.Enum

object CommonFailuresResultTest {
    object HttpStatus extends Enum {
        sealed case class EnumVal private[HttpStatus] (name: String, status: Int) extends Value with Failure.Reason

        val NotFound = EnumVal("NotFound", 404)

        val InternalServerError = EnumVal("InternalServerError", 500)
        val BadGateway = EnumVal("BadGateway", 502)
        val GatewayTimeout = EnumVal("GatewayTimeout", 504)
    }

    // A common failure to be used throughout the system.
    case class Detail(reason: HttpStatus.EnumVal, message: String, cause: Option[Throwable] = None) extends Failure.Detail

    case class User()
    trait Backend {
        def fetchUser(): Result[Option[User]]
    }

    class FrontendService(backend: Backend) {
        def userByMatch(): Result[User] = backend.fetchUser match {
            case Success(Some(user)) => Success(user)
            case Success(None)       => Failure(Detail(HttpStatus.NotFound, "User not found :("))
            case Failure(f)          => Failure(f)
        }

        def userByFlatMap(): Result[User] = {
            backend.fetchUser.flatMap {
                case Some(user) => Success(user)
                case None       => Failure(Detail(HttpStatus.NotFound, "User not found :("))
            }
        }

        def userByFlatMap2(): Result[User] = {
            import com.dadrox.scuttle.result.converters._
            backend.fetchUser.flatMap { _.asResult(Detail(HttpStatus.NotFound, "User not found :(")) }
        }

        def userByForComprehension(): Result[User] = {
            import com.dadrox.scuttle.result.converters._
            for {
                userOpt <- backend.fetchUser
                user <- userOpt.asResult(Detail(HttpStatus.NotFound, "User not found :("))
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
        test(fn) mustMatch { case Failure(Detail(HttpStatus.NotFound, _, _)) => }
    }

    @Test
    def match_timeout {
        backend.fetchUser --> Failure(Detail(HttpStatus.GatewayTimeout, "for test"))
        test(fn) mustMatch { case Failure(Detail(HttpStatus.GatewayTimeout, _, _)) => }
    }

    @Test
    def match_badJson {
        backend.fetchUser --> Failure(Detail(HttpStatus.BadGateway, "for test"))
        test(fn) mustMatch { case Failure(Detail(HttpStatus.BadGateway, _, _)) => }
    }
}

