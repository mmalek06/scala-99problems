package NinetyNineProblems

import scala.annotation.tailrec

object ListDuplicatesRemoval extends App {
  @tailrec
  def compressTailRecursive[T](l: List[T], current: T, result: List[T] = Nil): List[T] =
    if (l.isEmpty) result
    else if (current != l.head) compressTailRecursive(l.tail, l.head, result :+ l.head)
    else compressTailRecursive(l.tail, l.head, result)

  def compressFolding[T](l: List[T]) =
    l.foldRight(List[T]())((element, list) =>
      if (list.isEmpty || element != list.head) element +: list
      else list
    )

  val l = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

  println(compressTailRecursive(l, ""))
  println(compressFolding(l))
}
