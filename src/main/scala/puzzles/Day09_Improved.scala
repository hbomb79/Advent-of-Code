package puzzles

import main.Puzzle

/** Improved solution for Day 9
  */

object PuzzleNineImproved extends Puzzle {
  private type Movement = Point => Point
  private type Rope = Seq[Point]

  override def partOne(fileLines: Seq[String]): Unit = {
    println(
      applyMovements(extractMovements(fileLines), Seq.fill(2)(Point(0, 0))).size
    )
  }
  override def partTwo(fileLines: Seq[String]): Unit = {
    println(
      applyMovements(
        extractMovements(fileLines),
        Seq.fill(10)(Point(0, 0))
      ).size
    )
  }

  private def applyMovements(
      movements: Seq[Movement],
      rope: Rope
  ): Set[Point] = movements match {
    case Nil => Set.empty
    case (move :: rest) =>
      val r = applyMovement(move, rope)
      applyMovements(rest, r) + r.last
  }

  private def extractMovements(lines: Seq[String]): Seq[Movement] =
    lines.flatMap { case s"$dir $qty" =>
      Seq.fill(qty.toInt)(dir match {
        case "D" => p => p.copy(y = p.y + 1)
        case "U" => p => p.copy(y = p.y - 1)
        case "R" => p => p.copy(x = p.x + 1)
        case "L" => p => p.copy(x = p.x - 1)
      })
    }

  private def applyMovement(movement: Movement, rope: Rope): Rope =
    (0 to rope.length - 1).foldLeft(rope)((acc, idx) =>
      val current = acc(idx)
      val mvmt =
        if (idx == 0) movement
        else calcFollowMovement(current, acc(idx - 1))

      acc.updated(idx, mvmt(current))
    )

  def calcFollowMovement(current: Point, target: Point): Movement = {
    val dX = target.x - current.x
    val dY = target.y - current.y

    if (math.abs(dX) <= 1 && math.abs(dY) <= 1) identity
    else p => p.copy(x = p.x + dX.sign, y = p.y + dY.sign)
  }

  sealed case class Point(x: Int, y: Int)
}
