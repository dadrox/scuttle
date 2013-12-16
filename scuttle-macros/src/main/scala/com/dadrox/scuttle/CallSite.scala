package com.dadrox.scuttle

import scala.reflect.macros.Context

case class CallInfo(
        enclosingClass: String,
        enclosingMethod: Option[String],
        file: String,
        line: Int) {
    private lazy val method = enclosingMethod match {
        case Some(method) => s".$method()"
        case None         => ""
    }
    override lazy val toString = s"${enclosingClass}${method} @ ${file}:${line}"
}

object CallInfo {
    import language.experimental.macros
    implicit def callSite: CallInfo = macro CallSiteMacro.callSiteImpl
}

object CallSiteMacro {
    def callSiteImpl(c: Context): c.Expr[CallInfo] = {
        import c._
        import universe._

        val method = scala.util.Try(enclosingMethod.symbol.name.decoded).toOption match {
            case Some(s) => reify(Some(literal(s).splice))
            case None    => reify(None)
        }

        reify {
            CallInfo(
                literal(enclosingClass.symbol.fullName).splice,
                method.splice,
                literal(enclosingPosition.source.file.name).splice,
                literal(enclosingPosition.line).splice)
        }
    }
}
