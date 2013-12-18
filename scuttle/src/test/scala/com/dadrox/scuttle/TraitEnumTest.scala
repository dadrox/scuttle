package com.dadrox.scuttle

import org.junit.Test
import org.fictus.Fictus

object TraitEnum extends Enum {
    sealed trait EnumVal extends Value

    val A = new EnumVal { override val name = "A" }
    val B = new EnumVal { override val name = "B" }
}

class TraitEnumTest extends Fictus {
    import TraitEnum._

    @Test
    def hashcode {
        A.## mustNotEqual B.##
    }

    @Test
    def equals {
        A mustNotEqual B
        A mustEqual A
    }

    @Test
    def withName_works {
        withName("A").get mustEqual A
        withName("a") mustEqual None

        withName("B").get mustEqual B
        withName("b") mustEqual None

        withName("C") mustEqual None
        withName("") mustEqual None
        withName(null) mustEqual None
    }

    @Test
    def withNameIgnoringCase_works {
        withNameIgnoringCase("A").get mustEqual A
        withNameIgnoringCase("a").get mustEqual A

        withNameIgnoringCase("B").get mustEqual B
        withNameIgnoringCase("b").get mustEqual B

        withNameIgnoringCase("C") mustEqual None
        withNameIgnoringCase("") mustEqual None
        withNameIgnoringCase(null) mustEqual None
    }

    @Test
    def exhaustive_match_DOES_NOT_WORK_for_trait_based_enums {

        shouldThrow[MatchError] {
            val instance: TraitEnum.EnumVal = TraitEnum.A
            instance match { case TraitEnum.B => }
        }
    }

    @Test
    def bookkeepingWorks {
        A mustNotEqual B

        A.name mustEqual "A"
        A.toString mustEqual "EnumVal.A"
        B.name mustEqual "B"
        B.toString mustEqual "EnumVal.B"

        values mustEqual Vector(A, B)
    }
}