package boczula.mateusz.palindromes

import boczula.mateusz.palindromes.Palindromes.Palindrome
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

class PalindromesTest extends FlatSpec with Matchers {

  it should "find a palindrome when inputs satisfies the property" in {
    Palindromes.getPalindrome(Option("d"), "abc", "cba") should be ("abcdcba")
  }

  it should "find a palindrome when only the partial of the string is" in {
    Palindromes.getPalindrome(Option("d"), "abc", "cbe") should be ("bcdcb")
  }

  it should "should find a single palindrome" in {
    val palindromes = Palindromes.findPalindromes("abcdcba")
    palindromes should be (Seq(Palindrome(0, "abcdcba")))
  }

  it should "find all palindromes in the sample string" in {
    val palindromes = Palindromes.findPalindromes("sqrrqabccbatudefggfedvwhijkllkjihxymnnmzpop")
    palindromes should be (Seq(
      Palindrome(23, "hijkllkjih"), Palindrome(13, "defggfed"), Palindrome(5, "abccba")))
  }

  it should "sort palindromes by length descending before returning results" in {
    val input = generatePalindromeInput((0 to 100).toList)
    val palindromeLengths = Palindromes.findPalindromes(input).map(_.length).toList
    palindromeLengths should be (List(100, 99, 98))
  }

  private def generatePalindromeInput(palindromeLengths: List[Int]): String = {
    palindromeLengths.map(generatePalindrome) map(p => "!@#" + p + "!@#") reduce(_ + _)
  }

  private def generatePalindrome(length: Int): String = {
    if(length % 2 == 0) {
      val halfPalindrome = Random.alphanumeric.take (length / 2).mkString
      halfPalindrome + halfPalindrome.reverse
    }
    else {
      val halfPalindrome = Random.alphanumeric.take ((length-1) / 2).mkString
      halfPalindrome + Random.alphanumeric.take(1).mkString + halfPalindrome.reverse
    }
  }

}
