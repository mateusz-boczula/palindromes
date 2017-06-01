package boczula.mateusz.palindromes

object Palindromes {

  def main(args: Array[String]): Unit = {
    if(args.length != 1) {
      println("Wrong number of arguments: 1 expected")
    }
    else if (args(0) == null || args(0).isEmpty) {
      println("Provided string to search empty")
    }
    else {
      findPalindromes(args(0)).foreach { p =>
        println(s"Text: ${p.value}, Index: ${p.start}, Length: ${p.length}")}
    }
  }

  def findPalindromes(input: String) : Seq[Palindrome] = {
    val oddLengthPalindromes = findOddLengthPalindromes(input)
    val evenLengthPalindromes = findEvenLengthPalindromes(input)

    (evenLengthPalindromes ++ oddLengthPalindromes).sortBy(_.length * -1).take(3)
  }

  private def findEvenLengthPalindromes(input: String) = {
    input.zip(input.substring(1))
      .zipWithIndex
      .filter(p => p._1._1 == p._1._2)
      .map(_._2 + 1)
      .map(i => {
        val prefix = input.substring(0, i)
        val postfix = input.substring(i)
        val palindrome = getPalindrome(None, prefix, postfix)
        val halfLength = palindrome.length / 2
        Palindrome(i - halfLength, palindrome)
      })
  }

  private def findOddLengthPalindromes(input: String) = {
    input.zip(input.substring(2))
      .zipWithIndex
      .filter(p => p._1._1 == p._1._2)
      .map(_._2 + 1)
      .map(i => {
        val prefix = input.substring(0, i)
        val postfix = input.substring(i + 1)
        val palindrome = getPalindrome(Option(input.charAt(i).toString), prefix, postfix)
        val halfLength = (palindrome.length - 1) / 2
        Palindrome(i - halfLength, palindrome)
      })
  }

  def getPalindrome(cut: Option[String], prefix: String, postfix: String): String = {
    val halfPalindrome = prefix.reverse.zip(postfix).takeWhile(p => p._1 == p._2).map(_._1).mkString
    halfPalindrome.reverse + cut.getOrElse("") + halfPalindrome
  }

  case class Palindrome(start: Int, value: String) {
    def length() : Int = value.length
  }

}
