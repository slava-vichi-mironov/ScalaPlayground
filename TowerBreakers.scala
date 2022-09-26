object TowerBreakers {

    /*
     * Complete the 'towerBreakers' function below.
     *
     * The function is expected to return an INTEGER.
     * The function accepts INTEGER_ARRAY arr as parameter.
     */

  def towerBreakers(arr: Array[Int]): Int = {
    // Write your code here
    if (arr.map(primeFactors(_).size).reduce(_ ^ _) > 0) 1 else 2
  }

  def primeFactors(n: Int): List[Int] = {
    @tailrec
    def primeFactorsEvenTailrec(devisor: Int, remainder: Int, factors: List[Int]): (Int, List[Int]) = {
      if (remainder % devisor != 0) (remainder, factors)
      else primeFactorsEvenTailrec(devisor, remainder / devisor, factors :+ devisor)
    }
    val (nEven, twos) = primeFactorsEvenTailrec(2, n, List())
    val (nOdds, almostAllFactors) = Range.Int(3, Math.sqrt(nEven).toInt + 1, 2).foldLeft((nEven, twos)){
      (acc, devisor) => primeFactorsEvenTailrec(devisor, acc._1, acc._2)
    }

    if (nOdds > 2) almostAllFactors :+ nOdds
    else almostAllFactors
  }
}
