package com.dadrox.scuttle

import org.junit.Test
import org.fictus.Fictus

object TrivialCaseClassEnum extends Enum {
    sealed case class EnumVal private[TrivialCaseClassEnum] (name: String) extends Value

    val A = new EnumVal("A")
    val B = new EnumVal("B")
}

class TrivialCaseClassEnumTest extends Fictus {
    import TrivialCaseClassEnum._

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
    def bookkeeping_works {
        A mustNotEqual B

        A.name mustEqual "A"
        A.toString mustEqual "EnumVal(A)"
        B.name mustEqual "B"
        B.toString mustEqual "EnumVal(B)"

        values mustEqual Vector(A, B)
    }

    @Test
    def no_exhaustive_match_for_case_class_EnumVals { // :(
        shouldThrow[MatchError] {
            val instance: TrivialCaseClassEnum.EnumVal = TrivialCaseClassEnum.A
            instance match { case TrivialCaseClassEnum.B => }
        }
    }

    @Test
    def no_exhaustive_match_with_just_a_sealed_trait { // :(

        object SealedCaseClassEnum extends Enum {
            sealed trait Sealed
            case class EnumVal private[SealedCaseClassEnum] (name: String) extends Value

            val A = new EnumVal("A") with Sealed
            val B = new EnumVal("B") with Sealed
        }

        shouldThrow[MatchError] {
            val instance: SealedCaseClassEnum.EnumVal = SealedCaseClassEnum.A
            instance match { case SealedCaseClassEnum.B => }
        }
    }

    @Test
    def no_actual_exhaustive_match_if_you_seal_both_the_case_class_and_a_trait_ALL_LIES { // AND verbose :(

        object SealedCaseClassEnumWithSealedTrait extends Enum {
            sealed trait Sealed
            sealed case class EnumVal private[SealedCaseClassEnumWithSealedTrait] (name: String) extends Value

            val A = new EnumVal("A") with Sealed
            val B = new EnumVal("B") with Sealed
            val C = new EnumVal("C") with Sealed
        }

        val instance: SealedCaseClassEnumWithSealedTrait.EnumVal = SealedCaseClassEnumWithSealedTrait.A
        shouldThrow[MatchError] {
            instance match {
                case SealedCaseClassEnumWithSealedTrait.B =>
            }
        }

        instance match {
            case it @ SealedCaseClassEnumWithSealedTrait.A => it mustEqual SealedCaseClassEnumWithSealedTrait.A
            case it @ SealedCaseClassEnumWithSealedTrait.B => it mustEqual SealedCaseClassEnumWithSealedTrait.B
            case it @ SealedCaseClassEnumWithSealedTrait.C => it mustEqual SealedCaseClassEnumWithSealedTrait.C
        }
    }
}