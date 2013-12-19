package com.dadrox.scuttle.result

/** Represents nothing, but is a specific type that is completely incompatible with other types,
 *  whereas Nothing can stand in for any type and any type can stand in for Unit, both of which can "swallow" types
 *  This is useful for Result[Void] or Future[Void] where a Success or Failure is sufficient.
 */
sealed trait Void

/** The lone instance of Void */
case object Void extends Void