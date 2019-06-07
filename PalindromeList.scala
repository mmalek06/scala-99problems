package NinetyNineProblems

object PalindromeList extends App {
  def isPalindrome(l: List[Int]) = {
    val maxIdx = l.length / 2
    val add = if (l.length % 2 == 0) 0 else 1
    val half = l.take(maxIdx + add)
    val lRev = l.drop(maxIdx).reverse

    half == lRev
  }

  println(isPalindrome(List(1, 2, 3, 2, 1)))
  println(isPalindrome(List(1, 2, 3)))
  println(isPalindrome(List(1)))
  println(isPalindrome(List(3, 3)))
  println(isPalindrome(List(3, 3, 3, 3)))
  println(isPalindrome(List(3, 3, 3, 3, 5)))
}
