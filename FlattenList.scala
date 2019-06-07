package NinetyNineProblems

object FlattenList extends App{
  def flatten(l: List[Any]): List[Any] =
    l.flatMap {
      case l: List[_] => flatten(l)
      case l => List(l)
    }

  val l = List(List(1, 1), 2, List(3, List(5, 8)))

  println(flatten(l))
}
