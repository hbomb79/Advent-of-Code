package puzzles

import scala.io.Source
import scala.util.Try
import scala.util.Failure.apply
import scala.util.Using
import scala.util.Success
import scala.util.Failure
import scala.util.matching.Regex
import main.Puzzle

/** Advent of Code 2022 - Puzzle Four
  *
  * Given a file which has a multiple lists of numbers, each list/line
  * containing two pairs, output the number of times that a pair fully contains
  * the other pair, and the number of times a pair only partially intersects
  * with another
  */

object PuzzleFour extends Puzzle {
  override def run(filepath: String): Unit = {
    val assignments = loadInputFromFile(filepath)

    val (fullOverlaps, partialOverlaps) = assignments.foldLeft((0, 0)) {
      case ((fullAcc, partialAcc), CleaningAssignment(first, second)) =>
        val fullDelta =
          if (CleaningRange.isFullyOverlapping(first, second)) 1 else 0
        val partialDelta =
          if (CleaningRange.isOverlapping(first, second)) 1 else 0

        (fullAcc + fullDelta, partialAcc + partialDelta)
    }

    println(s"Fully overlapping assignments: ${fullOverlaps}")
    println(s"Partially intersecting assignments: ${partialOverlaps}")
  }

  private def loadInputFromFile(
      filepath: String
  ): Seq[CleaningAssignment] = {
    val CleaningAreaRegexp = "(\\d+)\\-(\\d+),(\\d+)\\-(\\d+)".r
    Source
      .fromFile(filepath)
      .getLines()
      .map(line =>
        CleaningAreaRegexp.findFirstMatchIn(line) match {
          case None =>
            throw new IllegalArgumentException(
              "Regular expression extraction for cleaning area ID FAILED - No match"
            )
          case Some(regexMatch) =>
            (1 to 4).map(index => Integer.parseInt(regexMatch.group(index)))
        }
      )
      .map { case Seq(pairOneA, pairOneB, pairTwoA, pairTwoB) =>
        CleaningAssignment(
          CleaningRange(pairOneA, pairOneB),
          CleaningRange(pairTwoA, pairTwoB)
        )
      }
      .toSeq
  }
}

case class CleaningRange(start: Int, end: Int)
case class CleaningAssignment(first: CleaningRange, second: CleaningRange)

object CleaningRange {
  def isFullyOverlapping(
      rangeA: CleaningRange,
      rangeB: CleaningRange
  ): Boolean = {
    // Either A fully encloses B, or B fully encloses A
    rangeA.start <= rangeB.start && rangeA.end >= rangeB.end || rangeB.start <= rangeA.start && rangeB.end >= rangeA.end
  }

  def isOverlapping(
      rangeA: CleaningRange,
      rangeB: CleaningRange
  ): Boolean = {
    val a = (rangeA.start to rangeA.end).toArray
    val b = (rangeB.start to rangeB.end).toArray

    a.intersect(b).length != 0
  }
}
