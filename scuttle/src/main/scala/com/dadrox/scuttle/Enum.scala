package com.dadrox.scuttle

trait Enum {
    import java.util.concurrent.atomic.AtomicReference

    type EnumVal <: Value //This is a type that needs to be found in the implementing class

    /** May be overridden, which affects toString: Vector(NewName(name1)) rather than Vector(EnumVal(name1))
     */
    def name(): String = "EnumVal"

    def withName(name: String): Option[EnumVal] = values.find(_.name == name)
    def withNameIgnoringCase(name: String): Option[EnumVal] = values.find(_.name.equalsIgnoreCase(name))

    private val _values = new AtomicReference(Vector[EnumVal]()) //Stores our enum values

    //Adds an EnumVal to our storage, uses CCAS to make sure it's thread safe, returns the ordinal
    private final def addEnumVal(newVal: EnumVal): Int = {
        import _values.{ get, compareAndSet => CAS }
        val oldVec = get
        val newVec = oldVec :+ newVal

        if ((get eq oldVec) && CAS(oldVec, newVec)) newVec.indexWhere(_ eq newVal)
        else addEnumVal(newVal)
    }

    /** @return all the EnumVals that exist for this type
     */
    def values: Vector[EnumVal] = _values.get

    //This is the class that we need to extend our EnumVal type with, it does the book-keeping for us
    protected trait Value { self: EnumVal => // Enforce that no one mixes in Value in a non-EnumVal type

        def name: String

        private final val ordinal = addEnumVal(this) //Adds the EnumVal and returns the ordinal

        override def toString = this match {
            case product: Product => scala.runtime.ScalaRunTime._toString(this.asInstanceOf[Product]).replace("EnumVal", Enum.this.name)
            case _                => name
        }

        override def equals(other: Any) = other match {
            case any: Value => this.name == any.name
            case _          => false
        }

        override def hashCode = 31337 * (this.getClass.## + name.## + ordinal)
    }
}
