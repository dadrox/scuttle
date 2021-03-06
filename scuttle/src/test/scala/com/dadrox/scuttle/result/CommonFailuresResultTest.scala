package com.dadrox.scuttle.result

import com.dadrox.scuttle.Enum
import org.fictus.Fictus

object CommonFailuresResultTest {
    object HttpStatus extends Enum {
        sealed case class EnumVal private[HttpStatus] (name: String, status: Int) extends Value with Failure.Reason

        val NotFound = EnumVal("NotFound", 404)

        val InternalServerError = EnumVal("InternalServerError", 500)
        val BadGateway = EnumVal("BadGateway", 502)
        val GatewayTimeout = EnumVal("GatewayTimeout", 504)
    }

    case class User()
    trait Backend {
        def fetchUser(): Result[Option[User]]
    }

    class FrontendService(backend: Backend) {
        def userByMatch(): Result[User] = backend.fetchUser match {
            case Success(Some(user)) => Success(user)
            case Success(None)       => Failure(HttpStatus.NotFound, "User not found :(")
            case f: Failure          => f
        }

        def userByFlatMap(): Result[User] = {
            backend.fetchUser.flatMap {
                case Some(user) => Success(user)
                case None       => Failure(HttpStatus.NotFound, "User not found :(")
            }
        }

        def userByFlatMap2(): Result[User] = {
            import com.dadrox.scuttle.result._
            backend.fetchUser.flatMap { _.failOnNone(Failure(HttpStatus.NotFound, "User not found :(")) }
        }

        def userByForComprehension(): Result[User] = {
            import com.dadrox.scuttle.result._
            for {
                userOpt <- backend.fetchUser
                user <- userOpt.failOnNone(Failure(HttpStatus.NotFound, "User not found :("))
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
        test(fn) mustMatch { case Failure(HttpStatus.NotFound, _, _) => }
    }

    @Test
    def match_timeout {
        backend.fetchUser --> Failure(HttpStatus.GatewayTimeout, "for test")
        test(fn) mustMatch { case Failure(HttpStatus.GatewayTimeout, _, _) => }
    }

    @Test
    def match_badJson {
        backend.fetchUser --> Failure(HttpStatus.BadGateway, "for test")
        test(fn) mustMatch { case Failure(HttpStatus.BadGateway, _, _) => }
    }
}

