package boczula.mateusz.palindromes

import boczula.mateusz.palindromes.Palindromes.Palindrome
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by jabaar on 30.05.17.
  */
class PalindromesTest extends FlatSpec with Matchers {

  it should "find a palindrome when inputs complient" in {
    Palindromes.getPalindrome(Option("d"), "abc", "cba") should be ("abcdcba")
  }

  it should "find a palindrome when only the partial is" in {
    Palindromes.getPalindrome(Option("d"), "abc", "cbe") should be ("bcdcb")
  }

  it should "find a palindrome when only the partial is asdf" in {
    val palindromes = Palindromes.getPalindromes("abcdcba")
    palindromes should be (Seq(Palindrome(0, "abcdcba")))
  }

  it should "adf" in {
    val palindromes = Palindromes.getPalindromes("sqrrqabccbatudefggfedvwhijkllkjihxymnnmzpop")
    palindromes should be (Seq(
      Palindrome(23, "hijkllkjih"), Palindrome(13, "defggfed"), Palindrome(5, "abccba")))
  }

}
