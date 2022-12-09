package puzzles

import main.Puzzle
import scala.annotation.newMain
import scala.annotation.tailrec

object PuzzleNine extends Puzzle {
  override def partOne(fileLines: Seq[String]): Unit = {
    println(findTailPositionsOfRope(extractMoves(fileLines), 2).size)
  }
  override def partTwo(fileLines: Seq[String]): Unit = {
    println(findTailPositionsOfRope(extractMoves(fileLines), 10).size)
  }

  /** Given a set of moves for the rope, this method will apply all the moves to
    * a rope of the length provided (where all knots in the rope start stacked
    * on top of each other in the same position; 0,0).
    *
    * @param moves
    *   The moves to apply to the rope
    * @param ropeLength
    *   The number of knots in the rope
    * @return
    *   The set of distinct positions that the TAIL of the rope was ever seen to
    *   be at
    */
  private def findTailPositionsOfRope(
      moves: Seq[Char],
      ropeLength: Int
  ): Set[Point] = {
    moves
      .foldLeft((Seq.fill(ropeLength)(Point(0, 0)), Set.empty[Point])) {
        case ((parts, tailAcc), move) =>
          val newParts = applyMoveToParts(move, parts, 0)
          (newParts, tailAcc + newParts.last)
      }
      ._2
  }

  /** A recursive method that applies a specific move to all the parts of the
    * rope - starting at the head and moving it's way to the tail.
    *
    * The head of the rope will *always* change position due to the move,
    * however the following knots will only move if they are no longer adjacent
    * (incl. diagonally). If a knot does not need to move (because it's still
    * touching the knot ahead of it), then this method early-returns as no more
    * work is to be done.
    *
    * @param move
    *   The move to apply
    * @param parts
    *   The knots in the rope
    * @param idx
    *   The index of the knot we're currently applying the move to
    * @return
    *   The parts after the move has been applied
    */
  @tailrec
  private def applyMoveToParts(
      move: Char,
      parts: Seq[Point],
      idx: Int
  ): Seq[Point] = (move, parts, idx) match {
    case (move, head :: ps, 0) =>
      applyMoveToParts(move, parts.updated(0, head.move(move)), idx + 1)
    case (_, _, idx) if idx == parts.length =>
      parts
    case (move, ps, idx) if ps(idx - 1).isAdjacent(ps(idx)) =>
      parts
    case (move, ps, idx) =>
      val target = ps(idx - 1)
      val current = ps(idx)
      applyMoveToParts(
        move,
        parts.updated(idx, current.moveTowards(target)),
        idx + 1
      )
  }

  private def extractMoves(lines: Seq[String]) = lines.flatMap {
    case s"$dir $qty" =>
      Seq.fill(qty.toInt)(dir.charAt(0))
  }

  sealed case class Point(x: Int, y: Int) {
    def move(dir: Char) = {
      dir match {
        case 'U' => Point(x, y - 1)
        case 'D' => Point(x, y + 1)
        case 'R' => Point(x + 1, y)
        case 'L' => Point(x - 1, y)
      }
    }

    def isAdjacent(point: Point): Boolean =
      math.abs(point.x - this.x) <= 1 && math.abs(point.y - this.y) <= 1

    def moveTowards(point: Point): Point = {
      val dX = applyDelta(point.y - this.y, 'U', 'D')
      val dY = applyDelta(point.x - this.x, 'L', 'R')

      Seq(dX ++ dY).flatten
        .foldLeft(this)(_ move _)
    }

    private def applyDelta(delta: Int, lt: Char, gt: Char): Option[Char] = {
      delta match {
        case dY if dY < 0 => Some(lt)
        case dY if dY > 0 => Some(gt)
        case _            => None
      }
    }
  }
}
