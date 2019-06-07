package NinetyNineProblems

object Goldbach extends App {

  val primes = 2 #:: Stream.from(3,2).filter(isPrime)

  def isPrime(n: Int): Boolean =
    primes.takeWhile(p => p * p <= n).forall(n % _ != 0)

  implicit class _Goldbach(val value: Int) extends AnyVal {
    def goldbach = {
      val currentPrimes = primes.takeWhile(_ < value).toList

      def finder(l1: List[Int], l2: List[Int]): (Int, Int) =
        if (l2.isEmpty) finder(l1.tail, currentPrimes)
        else if (l1.head + l2.head == value) (l1.head, l2.head)
        else finder(l1, l2.tail)

      finder(currentPrimes, currentPrimes)
    }

    def prettyGoldbach =
      primes.takeWhile { _ < value } find { p => isPrime(value - p) } match {
        case None     => throw new IllegalArgumentException
        case Some(p1) => (p1, value - p1)
    }
  }

  println(28.goldbach)
  println(28.prettyGoldbach)

}
