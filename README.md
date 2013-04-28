# Scuttle

A set of useful scala utilities and patterns.

## Features

### string.converters

notNull, notEmpty, notBlank, asInt, asLong, etc.
return Options instead of throwing exceptions!

### Response monad

Provides monadic response in a consistent fashion, carrying detailed failure information.

### Enum

A convenient enum structure.

#### Example

    object Status extends Enum {
        sealed case class EnumVal private[Status] (name: String, status: Int) extends Value
    
        val NotFound = EnumVal("NotFound", 404)
        val ServiceUnavailable = EnumVal("ServiceUnavailable", 503)
    }
    
    scala> Status.values
    res0: Vector[Status.EnumVal] = Vector(EnumVal(NotFound,404), EnumVal(ServiceUnavailable,503))
    
    scala> Status("foo")
    res1: Option[Status.EnumVal] = None
    
    scala> Status("NotFound")
    res2: Option[Status.EnumVal] = Some(EnumVal(NotFound,404))
    
    scala> Status.NotFound
    res3: Status.EnumVal = EnumVal(NotFound,404)

## Dependency

### build.sbt

    libraryDependencies += "com.dadrox" % "scuttle" % "0.1"

## License

Copyright (C) 2012-2013, Christopher Wood (dadrox)

Published under [BSD 2-Clause License](http://opensource.org/licenses/BSD-2-Clause)

