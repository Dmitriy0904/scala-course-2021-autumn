package karazin.scala.users.group.week1.homework

import scala.annotation.tailrec
/**
 * Preface
 * Implement all the things with ???.
 * All implementations must be tail-recursive is possible.
 * Feel free to use internal methods/functions.
 * Feel free to adjust signatures of hacked methods/functions.
 *
 * 1. Boolean Operators
 * Required task
 * Implement eager boolean operations Not, And and Or.
 * Requirements:
 * a) the function should not use embedded boolean operations
 * b) the functions should be eager
 * c) the function should use `if` expression and `true` and `false` boolean literals 
 *
 * 2. Fermat Numbers
 * Required task
 * Implement function which should calculate n-th Fermat number:
 * Fn = 2 ^ (2 ^ n) + 1, n is non-negative
 * Requirements:
 * a) the function should not use multiplication and power operations
 * b) the functions should only use addition operation
 * For more details @see https://en.wikipedia.org/wiki/Fermat_number
 *
 * 3. Look-and-say Sequence
 * Required task
 * Implement function which should calculate n-th number of Look-and-say sequence
 * For more details @see https://en.wikipedia.org/wiki/Look-and-say_sequence
 *
 * 4. Kolakoski sequence
 * Optional super challenging task
 * Implement function which should calculate n-th number of Kolakoski sequence
 * For more details @see https://en.wikipedia.org/wiki/Kolakoski_sequence
 */

object Homework :

  object `Boolean Operators` :

    def not(b: Boolean): Boolean = if b then false else true

    def and(left: Boolean, right: => Boolean): Boolean = if left then right else false

    def or(left: Boolean, right: => Boolean): Boolean = if left then true else right

  end `Boolean Operators`


  object `Fermat Numbers` :

    val multiplication: (BigInt, BigInt) => BigInt = (first, second) => {

      @tailrec
      def mult(first: BigInt, second: BigInt, res: BigInt): BigInt = {
        if second == 0
          then res
        else
          mult(first, second - 1, res + first)
      }

      mult(first, second, 0)
    }

    val power: (BigInt, BigInt) => BigInt = (num, power) => {

      @tailrec
      def pow(num: BigInt, power: BigInt, res: BigInt): BigInt = {
        if power == 0
          then res
        else
          pow(num, power - 1, multiplication(num, power))
      }

      pow(num, power, 1)
    }

    val fermatNumber: Int => BigInt = n => power(2, power(2, n)) + 1

  end `Fermat Numbers`



  object `Look-and-say Sequence` :
    val lookAndSaySequenceElement: Int => BigInt = n => {

      //Find new look-and-say sequence number from given
      @tailrec
      def findNum(nums: List[Int], i: Int, res: String): BigInt = {
          if i == nums.length
            then BigInt(res)
          else{
            val quantity = countItems(nums, i, nums(i), 0)
            val newI = i + quantity
            val newRes = res.concat(quantity.toString()).concat(nums(i).toString)
            findNum(nums, newI, newRes)
          }
      }

      //Count items in array of numbers
      @tailrec
      def countItems(nums: List[Int], i: Int, item: Int, quantity: Int): Int = {
        if i == nums.length ||  nums(i) != item
          then quantity
        else
          countItems(nums, i + 1, item, quantity + 1)
      }

      //Find given n-th element of look-and-say sequence
      @tailrec
      def findNthElement(n: Int, counter: Int, res: BigInt): BigInt = {
          if counter == n
            then res
          else {
            val nums = res.toString().map(_.asDigit).toList
            val newElement = findNum(nums, 0, "")
            findNthElement(n, counter + 1, newElement)
          }
      }

      if n < 0
        then throw new IllegalArgumentException("Illegal argument with negative value: " + n)
      else
        findNthElement(n, 0, 1)
}

  end `Look-and-say Sequence`

end Homework