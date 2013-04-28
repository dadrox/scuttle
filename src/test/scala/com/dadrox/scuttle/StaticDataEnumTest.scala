package com.dadrox.scuttle

import org.junit.Test
import org.fictus.Fictus

object HttpErrorClass extends Enum {
    case class EnumVal private[HttpErrorClass] (name: String, code: Int, detail: String) extends Value

    val Informational = EnumVal("Informational", 100, "Informational")
    val Success = EnumVal("Success", 200, "Success")
    val Redirection = EnumVal("Redirection", 300, "Redirection")
    val ClientError = EnumVal("ClientError", 400, "Client Error")
    val ServerError = EnumVal("ServerError", 500, "Server Error")
}

object HttpError extends Enum {
    case class EnumVal private[HttpError] (name: String, code: Int, detail: String, genericClass: HttpErrorClass.EnumVal) extends Value

    val Continue = EnumVal("Continue", 100, "Continue", HttpErrorClass.Informational)
    val Ok = EnumVal("Ok", 200, "OK", HttpErrorClass.Success)
    val MultipleChoices = EnumVal("MultipleChoices", 300, "Multiple Choices", HttpErrorClass.Redirection)

    val BadRequest = EnumVal("BadRequest", 400, "Bad Request", HttpErrorClass.ClientError)
    val Forbidden = EnumVal("Forbidden", 403, "Forbidden", HttpErrorClass.ClientError)
    val NotFound = EnumVal("NotFound", 404, "Not Found", HttpErrorClass.ClientError)
    val Teapot = EnumVal("Teapot", 418, "I'm a teapot", HttpErrorClass.ClientError) // RFC 2324

    val InternalServerError = EnumVal("InternalServerError", 500, "Internal Server Error", HttpErrorClass.ServerError)
    val GatewayTimeout = EnumVal("GatewayTimeout", 504, "Gateway Timeout", HttpErrorClass.ServerError)
}

class StaticDataEnumTest extends Fictus {

    @Test
    def dataIsAllThere {
        HttpError.Teapot.code mustEqual 418
        HttpError.Teapot.detail mustEqual "I'm a teapot"
        HttpError.Teapot.genericClass mustEqual HttpErrorClass.ClientError

        HttpErrorClass.ClientError.code mustEqual 400
        HttpErrorClass.ClientError.detail mustEqual "Client Error"
    }

    @Test
    def matchingWorks {
        HttpError.BadRequest match {
            case incorrect @ HttpError.Continue => fail("Incorrectly matched " + incorrect)
            case HttpError.BadRequest           =>
            case incorrect                      => fail("Incorrectly matched " + incorrect)
        }
    }

    @Test
    def toString_is_stable {
        HttpError.Teapot.toString mustEqual "EnumVal(Teapot,418,I'm a teapot,EnumVal(ClientError,400,Client Error))"
    }
}