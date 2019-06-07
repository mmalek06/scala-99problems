package NinetyNineProblems

object ListPackDuplicates extends App {
  def packFolding[T](l: List[T]) =
    l.foldRight(List[List[T]]())((element, list) =>
      if (list.isEmpty || element != list.head.head) List(element) :: list
      else element +: list.head :: list.tail)

  val l = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

  println(packFolding(l))
}
