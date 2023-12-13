package puzzles

import scala.io.Source
import scala.util.Try
import scala.util.Failure.apply
import scala.util.Using
import scala.util.Success
import scala.util.Failure
import scala.util.matching.Regex
import main.Puzzle

object PuzzleFour extends Puzzle {
  override def partOne(lines: Seq[String]): Unit = {
    val fullyOverlapping = extractCleaningAssignments(lines).count(a =>
      CleaningRange.isFullyOverlapping(a.first, a.second)
    )

    println(s"Fully overlapping assignments: ${fullyOverlapping}")
  }

  override def partTwo(lines: Seq[String]): Unit = {
    val partiallyOverlapping = extractCleaningAssignments(lines).count(a =>
      CleaningRange.isOverlapping(a.first, a.second)
    )

    println(s"Partially overlapping assignments: ${partiallyOverlapping}")
  }

  private def extractCleaningAssignments(
      lines: Seq[String]
  ): Seq[CleaningAssignment] = {
    val CleaningAreaRegexp = "(\\d+)\\-(\\d+),(\\d+)\\-(\\d+)".r
    lines
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
