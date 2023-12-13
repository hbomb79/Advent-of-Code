package puzzles

import main.Puzzle
import scala.annotation.tailrec

object PuzzleFourteen extends Puzzle {
  val SandEntryPoint = Point(500, 0)
  override def partOne(fileLines: Seq[String]): Unit = {
    val lines = parse(fileLines)
    val maximumY = lines.map(_.y).max

    val restingSand = runSimulation(SandEntryPoint, lines, Set.empty, maximumY)(
      completionPredicate = _.isEmpty,
      moveFilter = _ => true
    )

    println(restingSand.size)
  }
  override def partTwo(fileLines: Seq[String]): Unit = {
    val lines = parse(fileLines)
    val maximumY = lines.map(_.y).max + 2

    val restingSand = runSimulation(SandEntryPoint, lines, Set.empty, maximumY)(
      completionPredicate = _.contains(SandEntryPoint),
      moveFilter = _.y < maximumY
    )

    println(restingSand.size)
  }

  @tailrec
  private def runSimulation(
      entryPoint: Point,
      wallPoints: Set[Point],
      existingSand: Set[Point],
      maximumY: Int
  )(implicit
      completionPredicate: Option[Point] => Boolean,
      moveFilter: Point => Boolean
  ): Set[Point] =
    simulateSand(
      entryPoint,
      wallPoints,
      existingSand,
      moveFilter,
      maximumY
    ) match {
      case p if completionPredicate(p) => existingSand ++ Seq(p).flatten
      case Some(restPoint) =>
        runSimulation(
          entryPoint,
          wallPoints,
          existingSand + restPoint,
          maximumY
        )
      case _ => throw new IllegalStateException("Unexpected simulation result")
    }

  @tailrec
  private def simulateSand(
      fallPoint: Point,
      blockedPoints: Set[Point],
      sandPoints: Set[Point],
      moveFilter: Point => Boolean,
      maximumY: Int
  ): Option[Point] = Seq(0, -1, 1)
    .map(dX => fallPoint.copy(x = fallPoint.x + dX, y = fallPoint.y + 1))
    .filter(moveFilter)
    .find(a => !sandPoints.contains(a) && !blockedPoints.contains(a))
    .match {
      case None                              => Some(fallPoint)
      case Some(Point(_, y)) if y > maximumY => None
      case Some(point) =>
        simulateSand(point, blockedPoints, sandPoints, moveFilter, maximumY)
    }

  private def parse(lines: Seq[String]): Set[Point] =
    lines.foldLeft(Set.empty[Point])((blockedPoints, line) =>
      formLines(extractPoints(line, Seq.empty), blockedPoints)
    )

  @tailrec
  private def extractPoints(line: String, points: Seq[Point]): Seq[Point] =
    line match {
      case s if s.isEmpty => points
      case s if s.startsWith("->") =>
        extractPoints(s.drop(3), points)
      case _ =>
        val first = line.takeWhile(_.isDigit)
        val second = line.drop(first.length + 1).takeWhile(_.isDigit)
        val rest = line.drop(first.length + second.length + 2)

        extractPoints(rest, points :+ Point(first.toInt, second.toInt))
    }

  @tailrec
  private def formLines(
      lineVertices: Seq[Point],
      filledPoints: Set[Point]
  ): Set[Point] = lineVertices match {
    case _ +: Nil =>
      filledPoints
    case first +: second +: rest =>
      val n = if (first.x == second.x) {
        val ps = Seq(first.y, second.y)
        (ps.min to ps.max).map(y => Point(first.x, y))
      } else {
        val ps = Seq(first.x, second.x)
        (ps.min to ps.max).map(x => Point(x, first.y))
      }

      formLines(second +: rest, filledPoints ++ n)
  }

  sealed case class Point(x: Int, y: Int)
}
