package puzzles

import main.Puzzle
import scala.annotation.tailrec

object PuzzleFifteen extends Puzzle {
  override def partOne(fileLines: Seq[String]): Unit = {
    val yLine = 2_000_000
    val sensors = parseSensors(fileLines)
    val ranges = findRangesForSensors(sensors, yLine, Seq.empty)

    println(
      ranges.foldLeft(0L)((acc, range) =>
        acc + Math.abs(range.end - range.start)
      )
    )
  }

  override def partTwo(fileLines: Seq[String]): Unit = {
    val sensors = parseSensors(fileLines)
    val maxY = 4_000_000;

    (0 to maxY).view
      .map(y => (y, findRangesForSensors(sensors, y, Seq.empty)))
      .find((y, mergedRanges) => mergedRanges.length == 2) match
      case None                    => throw new IllegalStateException()
      case Some((y, mergedRanges)) =>
        // A line, with all it's ranges merged, could not be completely
        // covered by only one range (i.e. there is a GAP!!)
        println((mergedRanges.head.end + 1) * maxY + y)

  }

  @tailrec
  private def findRangesForSensors(
      sensors: Seq[Sensor],
      y: Int,
      ranges: Seq[Range]
  ): Seq[Range] = sensors match {
    case Nil => mergeRanges(ranges.sortBy(_.start))
    case (sensor :: next) =>
      sensor.getEffectiveRangeAt(y) match {
        case Some(r) =>
          findRangesForSensors(
            next,
            y,
            expandRanges(ranges, r)
          )
        case _ =>
          findRangesForSensors(next, y, ranges)
      }
  }

  private def expandRanges(ranges: Seq[Range], newRange: Range): Seq[Range] = {
    ranges.zipWithIndex.find((a, _) => a.overlaps(newRange)) match {
      case None                     => ranges :+ newRange
      case Some(overlapping, index) =>
        // This range overlaps, create a new range which expands to
        // hold the new range too
        val expandedRange = Range(
          Math.min(newRange.start, overlapping.start),
          Math.max(newRange.end, overlapping.end)
        )

        expandRanges(
          ranges.take(index) ++ ranges.drop(index + 1),
          expandedRange
        )
    }
  }

  // NOTE: Expects input ranges are SORTED in ascending order based on their 'start'
  private def mergeRanges(ranges: Seq[Range]): Seq[Range] = {
    ranges.drop(1).foldLeft(Seq(ranges(0))) { (acc, r) =>
      val l = acc.last
      if (r.start == l.end + 1) {
        // Touching, replace last range with a merged version
        acc.dropRight(1) :+ Range(l.start, r.end)
      } else {
        // Not touching, cannot merge
        acc :+ r
      }
    }
  }

  private def parseSensors(lines: Seq[String]): Seq[Sensor] = lines.map {
    case s"Sensor at x=$sX, y=$sY: closest beacon is at x=$bX, y=$bY" =>
      val sensor = Point(sX.toInt, sY.toInt)
      val beacon = Point(bX.toInt, bY.toInt)

      Sensor(sensor, beacon, beacon.manhattenDistance(sensor))
  }

  case class Range(start: Long, end: Long) {
    def overlaps(r: Range): Boolean =
      start <= r.end && r.start <= end
  }

  case class Point(x: Long, y: Long) {
    def manhattenDistance(other: Point): Long =
      Math.abs(other.x - x) + Math.abs(other.y - y)
  }

  case class Sensor(position: Point, closestBeacon: Point, dist: Long) {
    def getEffectiveRangeAt(y: Int): Option[Range] = {
      dist - Math.abs(position.y - y) match {
        case reach if reach < 0 => None
        case reach =>
          Some(
            Range(position.x - reach, position.x + reach)
          )
      }
    }
  }
}
