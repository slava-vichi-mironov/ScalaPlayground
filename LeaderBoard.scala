package com.slavasversion.various

import scala.annotation.tailrec
import scala.collection.mutable.SortedMap


//https://www.hackerrank.com/challenges/climbing-the-leaderboard/problem
object LeaderBoard extends App{

  def climbingLeaderboard(ranked: Array[Int], player: Array[Int]): Array[Int] = {

    @tailrec
    def buildTree(rankedLeft: Array[Int], lastScoreRanked: Int, currRank: Int, acc: SortedMap[Int, Int]): SortedMap[Int, Int] = {
      if (rankedLeft.isEmpty) acc
      else if (rankedLeft.head < lastScoreRanked)
        buildTree(rankedLeft.tail, rankedLeft.head, currRank + 1, acc + (rankedLeft.head -> (currRank + 1)))
      else
        buildTree(rankedLeft.tail, rankedLeft.head, currRank, acc)
    }

    val tree = buildTree(ranked.tail, ranked.head, 1, SortedMap[Int, Int]((ranked.head-> 1)))
    player.map(x => {
      if (tree.contains(x))
        tree(x)
      else
        tree.minAfter(x).getOrElse((0, 0))._2 + 1
    }
    )
  }

  val bla = climbingLeaderboard(Array(100, 90, 90, 80), Array(70, 80, 105))

  print(bla.mkString(" "))
}