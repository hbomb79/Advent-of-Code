package puzzles

import main.Puzzle
import scala.annotation.tailrec
import scala.compiletime.ops.boolean
import IterHelper.countUntil

object PuzzleEight extends Puzzle {
  type Axis = Seq[Seq[Int]]
  type ProducerFn[T] = (grid: Grid, point: Point) => T

  override def partOne(fileLines: Seq[String]): Unit = {
    println(runOnNonEdges(fileLines, isTreeVisible, true).count(_ == true))
  }

  override def partTwo(fileLines: Seq[String]): Unit = {
    println(runOnNonEdges(fileLines, getScenicScore, 0).max)
  }

  /** Returns the "scenic score" for a given point, which is found by
    * calculating how far in each of the four directions of the grid (up, down,
    * left, right) we can move from this point before reaching a point which has
    * a value equal or higher than the value of the point we started on.
    *
    * The scores calculated for each direction are then multiplied together and
    * returned
    *
    * Note: This method will fail if a point provided is on the edge of the
    * graph. For this reason the method should be applied using
    * [[runOnNonEdges]].
    */
  private def getScenicScore(
      grid: Grid,
      point: Point
  ): Int = {
    val height = grid.at(point)
    val (left, right, above, below) =
      grid.rays(point)

    Seq(left.reverse, right, above.reverse, below)
      .map(countUntil(_, height))
      .reduce(_ * _)
  }

  /** Tests if the point provided can "see" the edge of the grid in at least one
    * of the four directions (up, down, left, right). A point can "see" the edge
    * if none of the points between it and said edge has an equal or higher
    * value.
    *
    * Note: This method will fail if a point provided is on the edge of the
    * graph. For this reason the method should be applied using
    * [[runOnNonEdges]].
    */
  private def isTreeVisible(
      grid: Grid,
      point: Point
  ): Boolean = {
    val height = grid.at(point)
    val (left, right, above, below) =
      grid.rays(point)

    left.max < height
    || right.max < height
    || above.max < height
    || below.max < height
  }

  /** Given lines representing a grid this method will call a provided producer
    * function on all points in the grid that do NOT lie on the edge of the
    * grid. The value returned by the test function is included in the output.
    *
    * For points that lie on the edge, the default value provided is included in
    * the output instead.
    *
    * @param lines
    *   The lines to parse for the grid
    * @param producer
    *   Function to run on all non-edge points to get value
    * @param default
    *   Value to use for all edge points
    * @return
    *   All values calculated for all points
    */
  private def runOnNonEdges[T](
      lines: Seq[String],
      producer: ProducerFn[T],
      defaultValue: T
  ): Seq[T] = {
    val width = lines(0).length
    val rows = lines.map(_.map(_.asDigit))
    val grid = Grid(rows, rows.transpose)
    (0 to (width * lines.length) - 1).map { index =>
      Point(index / width, index % width) match {
        // Edge cases (haha, get it)
        case Point(0, _) | Point(_, 0)                      => defaultValue
        case Point(rowIdx, _) if rowIdx == lines.length - 1 => defaultValue
        case Point(_, colIdx) if colIdx == width - 1        => defaultValue
        case point => producer(grid, point)
      }
    }
  }

  case class Point(x: Int, y: Int)
  case class Grid(rows: Axis, cols: Axis) {
    def at(point: Point): Int =
      rows(point.y)(point.x)

    def rays(
        point: Point
    ): (Seq[Int], Seq[Int], Seq[Int], Seq[Int]) = {
      val hor = rows(point.y).splitAt(point.x)
      val ver = cols(point.x).splitAt(point.y)

      (hor._1, hor._2.drop(1), ver._1, ver._2.drop(1))
    }
  }
}
