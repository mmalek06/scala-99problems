package NinetyNineProblems

object ReverseList extends App {
  def reverse[A](l: List[A]) = {
    def reverseLocal(lReversed: List[A], lOriginal: List[A]): List[A] =
      if (lOriginal.isEmpty) lReversed
      else reverseLocal(lOriginal.head +: lReversed, lOriginal.tail)

    reverseLocal(Nil, l)
  }

  val li = List(1, 2, 3)
  val reversed = reverse(li)

  println(reversed)
}
