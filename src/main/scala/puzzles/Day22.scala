package puzzles

import main.Puzzle
import scala.annotation.tailrec

object PuzzleTwentyTwo extends Puzzle {
  override def partOne(fileLines: Seq[String]): Unit = {
    val (map, instructions) = parse(fileLines)
    val finMap = instructions.foldLeft(map)((map, instruction) =>
      map.executeInstruction(instruction)
    )

    val row = finMap.position.x
    val col = finMap.position.y
    val facing = finMap.facing match
      case Direction.N => 3
      case Direction.E => 0
      case Direction.S => 1
      case Direction.W => 2

    println((1000 * col) + (4 * row) + facing)

  }
  override def partTwo(fileLines: Seq[String]): Unit = {
    // Haha lol fuckthat...
  }

  enum Direction:
    case N, E, S, W

    def rotate(anticlockwise: Boolean): Direction =
      if anticlockwise then
        Direction.fromOrdinal(Math.floorMod(this.ordinal - 1, 4))
      else Direction.fromOrdinal((this.ordinal + 1) % 4)

  sealed trait Instruction
  case class Turn(anti: Boolean) extends Instruction
  case class Move(n: Int) extends Instruction

  case class Point(x: Int, y: Int):
    def shift(dir: Direction) = dir match
      case Direction.N => copy(y = y - 1)
      case Direction.E => copy(x = x + 1)
      case Direction.S => copy(y = y + 1)
      case Direction.W => copy(x = x - 1)

  case class Row(offset: Int, data: Seq[Boolean]):
    def end: Int = offset + data.size
    def contains(x: Int): Boolean = offset < x && end + 1 > x

  case class Map(position: Point, facing: Direction, rows: Seq[Row]) {
    def get(point: Point): Option[Boolean] = {
      if point.y < 1 || point.y > rows.length then None
      else
        val row = rows(point.y - 1)
        val x = point.x - row.offset
        if x < 1 || x > row.data.length then None
        else Some(row.data(x - 1))
    }

    private def executeMoves(n: Int, point: Point): Point = {
      if n == 0 then point
      else
        val next = point.shift(facing)
        get(next) match {
          case None =>
            val wrapped = facing match {
              case Direction.N =>
                next.copy(y =
                  rows.zipWithIndex.reverse
                    .find((r, idx) => r.contains(next.x))
                    .get
                    ._2 + 1
                )
              case Direction.S =>
                next.copy(y = rows.indexWhere(_.contains(next.x)) + 1)
              case Direction.E =>
                next.copy(x = rows(next.y - 1).offset + 1)
              case Direction.W =>
                next.copy(x = rows(next.y - 1).end)
            }

            get(wrapped) match
              case Some(false) => point
              case Some(true)  => executeMoves(n - 1, wrapped)
              case None        => throw new IllegalStateException

          case Some(true)  => executeMoves(n - 1, next)
          case Some(false) => point
        }
    }

    def executeInstruction(instr: Instruction): Map = instr match {
      case Turn(anti) => copy(facing = facing.rotate(anti))
      case Move(n)    => copy(position = executeMoves(n, position))
    }
  }

  private def parse(lines: Seq[String]): (Map, List[Instruction]) = {
    val (mapDef, instrDef) = lines.span(_.nonEmpty)
    val m = mapDef.map { line =>
      val (before, after) = line.span(_.isWhitespace)
      val data = after.map {
        case '#' => false
        case '.' => true
        case _   => throw new IllegalStateException
      }

      Row(before.length, data)
    }

    val i = Iterator
      .unfold[Instruction, String](instrDef.tail.head) { str =>
        if str.isEmpty then None
        else
          Some(str.head match {
            case 'L' => (Turn(true), str.tail)
            case 'R' => (Turn(false), str.tail)
            case _ =>
              val (before, after) = str.span(_.isDigit)
              (Move(before.toInt), after)
          })
      }
      .toList

    val startPoint = Point(m.head.offset + m.head.data.indexWhere(_ == true), 1)
    (Map(startPoint, Direction.E, m), i)
  }
}
