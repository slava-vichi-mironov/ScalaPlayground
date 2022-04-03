package com.slavasversion.various

import scala.annotation.tailrec

object KnuthMorrisPrattAlgorithm {
  def knuthMorrisPrattAlgorithm(text: String, pattern: String): List[Int] = {
    @tailrec
    def tailRecKnuthMorrisPrattAlgorithm(t: Int, p: Int, matches: List[Int], table: Array[Int]): List[Int] = {
      if (t >= text.length) matches
      else {
        if (pattern.charAt(p) == text.charAt(t)) {
          if (p + 1 == pattern.length){ //match found
            tailRecKnuthMorrisPrattAlgorithm(t + 1, table(p + 1), (t - p) +: matches, table)
          }
          else //continue to match the pattern
            tailRecKnuthMorrisPrattAlgorithm(t + 1, p + 1, matches, table)
        }
        else {
          val newP = table(p)
          if (newP < 0)
            tailRecKnuthMorrisPrattAlgorithm(t + 1, newP + 1, matches, table)
          else
            tailRecKnuthMorrisPrattAlgorithm(t, newP, matches, table)
        }
      }
    }

    val table = buildPartialMatchTable(pattern)
    tailRecKnuthMorrisPrattAlgorithm(0, 0, List(), table)
  }

  def buildPartialMatchTable(pattern: String): Array[Int] ={
    @tailrec
    def tailRecBuildPartialMatchTable(prefixLen: Int, i: Int, table: Array[Int]): Array[Int] = {
      if (i >= pattern.length) table
      else {
        if (pattern.charAt(prefixLen) == pattern.charAt(i)){
          table(i + 1) = prefixLen + 1
          tailRecBuildPartialMatchTable(prefixLen + 1, i + 1, table)
        }
        else if (prefixLen > 0)
          tailRecBuildPartialMatchTable(table(prefixLen), i, table)
        else {
          table(i + 1) = 0
          tailRecBuildPartialMatchTable(prefixLen, i + 1, table)
        }
      }
    }

    val table = new Array[Int](pattern.length + 1); table(0) = -1; table(1) = 0
    tailRecBuildPartialMatchTable(0, 1, table)
  }
}
