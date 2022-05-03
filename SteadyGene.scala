package com.slavasversion.various

import scala.annotation.tailrec
import scala.collection.mutable

object SteadyGene {
  def steadyGene(gene: String): Int = {
    val maxAllowedPerChar: Int = gene.length / 4
    val charCount = gene.foldLeft(mutable.Map[Char, Int]()){
      (map, char) => {
        map(char) =  map.getOrElse(char, 0) + 1
        map
      }
    }

    def extrasAvailable(charCount: mutable.Map[Char, Int], maxAllowedPerChar: Int): Boolean =
      charCount.values.exists(count => count > maxAllowedPerChar)

    @tailrec
    def steadyGeneTailRec(gene: String, charCount: mutable.Map[Char, Int], leftPointer: Int, rightPointer: Int, minGeneLength: Int): Int = {
      if (leftPointer == 0 && rightPointer == 0 && !extrasAvailable(charCount, maxAllowedPerChar)) 0
      else if (rightPointer < gene.length) {
        if (extrasAvailable(charCount, maxAllowedPerChar)) {
          charCount(gene.charAt(rightPointer)) = charCount(gene.charAt(rightPointer)) - 1
          steadyGeneTailRec(gene, charCount, leftPointer, rightPointer + 1, minGeneLength)
        } else {
          charCount(gene.charAt(leftPointer)) = charCount(gene.charAt(leftPointer)) + 1
          if (extrasAvailable(charCount, maxAllowedPerChar))
            steadyGeneTailRec(gene, charCount, leftPointer + 1, rightPointer, scala.math.min(minGeneLength, rightPointer - (leftPointer + 1) + 1))
          else
            steadyGeneTailRec(gene, charCount, leftPointer + 1, rightPointer, minGeneLength)
        }
      }
      else minGeneLength
    }

    steadyGeneTailRec(gene, charCount, 0, 0, Int.MaxValue)
  }

}
