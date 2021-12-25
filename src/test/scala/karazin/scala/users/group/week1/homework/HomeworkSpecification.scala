package karazin.scala.users.group.week1.homework

import org.scalacheck._
import Prop.{forAll, propBoolean}
import Homework._
import karazin.scala.users.group.week1.homework.arbitraries

object HomeworkSpecification extends Properties("Homework"):

  include(BooleanOperatorsSpecification)
  include(FermatNumbersSpecification)
  include(LookAndAaSequenceSpecification)

end HomeworkSpecification

object BooleanOperatorsSpecification extends Properties("Boolean Operators"):
  import `Boolean Operators`._

  property("not") = forAll { (b: Boolean) =>
    not(b) == !b
  }

  property("and") = forAll { (pair: (Boolean, Boolean)) =>
    val (left, right) = pair
    
    and(left, right) == (left && right)
  }

  property("and operator with operand with exception") = propBoolean {
    and(false, throw new Exception()) == false
  }

  property("or") = forAll { (pair: (Boolean, Boolean)) =>
    val (left, right) = pair
    
    or(left, right) == (left || right)
  }

  property("or operator with operand with exception") = propBoolean {
    or(true, throw new Exception()) == true
  }

end BooleanOperatorsSpecification

object FermatNumbersSpecification extends Properties("Fermat Numbers"):
  import `Fermat Numbers`._
  import arbitraries.given Arbitrary[Int]

  property("multiplication") = forAll { (left: Int, right: Int) =>
    multiplication(left, right) == (left * right)
  }

  property("power") = forAll { (left: Int, right: Int) =>
    power(left, right) == (0 until right).foldLeft(BigInt(1)) { (acc, _) => acc * left }
  }

  property("fermatNumber") = forAll { (n: Int) =>
    fermatNumber(n) == Math.pow(2, Math.pow(2, 2)) + 1
  }  

end FermatNumbersSpecification

object LookAndAaSequenceSpecification extends Properties("Look-and-say Sequence"):
  import `Look-and-say Sequence`._
  import arbitraries.given Arbitrary[Int]

  property("`Look-and-say Sequence") = forAll { (n: Int) =>
    val sequenceElements: List[BigInt] = List(
      1, 11, 21, 1211, 111221,
      312211, 13112221, 1113213211,
      BigInt("31131211131221"),
      BigInt("13211311123113112211"),
      BigInt("11131221133112132113212221")
    )

    lookAndSaySequenceElement(n) == sequenceElements(n)
}

end LookAndAaSequenceSpecification
