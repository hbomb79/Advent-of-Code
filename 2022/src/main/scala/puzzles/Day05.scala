package puzzles
import main.Puzzle
import scala.io.Source
import scala.util.Using
import scala.collection.mutable.Stack

object PuzzleFive extends Puzzle {
  private def extractStacksAndMoves(
      lines: Seq[String]
  ): (Seq[List[Char]], Seq[Move]) = {
    val layoutLines = lines.takeWhile(_.nonEmpty).dropRight(1)
    val moveLines = lines.drop(layoutLines.length + 2)

    (processLayoutLines(layoutLines), processMoveLines(moveLines))
  }

  override def partOne(
      lines: Seq[String]
  ): Unit = {
    val (stacks, moves) = extractStacksAndMoves(lines)

    println(executeMovesPartOne(stacks, moves).map(_.head).mkString)
  }

  override def partTwo(
      lines: Seq[String]
  ): Unit = {
    val (stacks, moves) = extractStacksAndMoves(lines)

    println(executeMovesPartTwo(stacks, moves).map(_.head).mkString)
  }

  private def executeMovesPartTwo(
      stacks: Seq[List[Char]],
      moves: Seq[Move]
  ): Seq[List[Char]] = {
    moves.foldLeft(stacks) { case (acc, Move(qty, origin, dest)) =>
      val targetValues = acc(origin).take(qty)
      acc.zipWithIndex.map {
        case (stack, stackIndex) if stackIndex == origin =>
          stack.drop(qty)
        case (stack, stackIndex) if stackIndex == dest =>
          targetValues ++ stack
        case (stack, _) =>
          stack
      }
    }
  }

  private def executeMovesPartOne(
      stacks: Seq[List[Char]],
      moves: Seq[Move]
  ): Seq[List[Char]] = {
    moves
      .flatMap(m => Seq.fill(m.qty)(Tuple2(m.origin, m.dest)))
      .foldLeft(stacks) { case (acc, (origin, dest)) =>
        val targetValue = acc(origin).head

        acc.zipWithIndex.map {
          case ((_ :: tail), index) if index == origin => tail
          case (list, index) if index == dest          => targetValue +: list
          case (e, _)                                  => e
        }
      }
  }

  private def processMoveLines(moveLines: Seq[String]): Seq[Move] = {
    moveLines.map { line =>
      val nums = ("\\d+".r).findAllIn(line).map(_.toInt).toSeq
      Move(nums(0), nums(1) - 1, nums(2) - 1)
    }
  }

  private def processLayoutLines(
      layoutLines: Seq[String]
  ): Seq[List[Char]] = {
    val lineLength = layoutLines(0).length
    val widthInCrates: Int = (lineLength + 1) / 4
    val height = layoutLines.length
    val allCrates = layoutLines.mkString

    (0 to widthInCrates - 1).foldLeft(Seq.empty[List[Char]]) {
      case (acc, stackIndex) => {
        val charsInStack = (0 to height - 1).flatMap { shelfIndex =>
          val idx = (shelfIndex * lineLength) + (stackIndex * 4) + 1
          allCrates.charAt(idx) match {
            case c if c.isWhitespace => None
            case c                   => Some(c)
          }
        }

        acc :+ List.from(charsInStack)
      }
    }
  }

  case class Move(qty: Int, origin: Int, dest: Int)
}
