package puzzles
import main.Puzzle
import scala.io.Source
import scala.util.Using
import scala.collection.mutable

object PuzzleFive extends Puzzle {
  override def run(filepath: String): Unit = {
    loadInputFromFile(filepath)
  }

  private def loadInputFromFile(filepath: String): Seq[Any] = {
    Using(Source.fromFile(filepath)) { source =>
      val lines = source.getLines.toSeq
      val layoutLines = lines.takeWhile(_.nonEmpty).dropRight(1)
      val moveLines = lines.drop(layoutLines.length + 2)

      val stacks = processLayoutLines(layoutLines)
      val moves = processMoveLines(moveLines)

      partOne(stacks.map(mutable.Stack.from), moves)
      partTwo(stacks, moves)
    }.get

    Seq.empty
  }

  private def partOne(
      stacks: Seq[mutable.Stack[Char]],
      moves: Seq[Move]
  ): Unit = {
    val newStacks = executeMovesPartOne(stacks, moves)

    val tops = newStacks.map(_.top).mkString
    println(s"PART ONE: Message displayed by all crates at the top: $tops")

  }

  private def partTwo(
      stacks: Seq[mutable.Stack[Char]],
      moves: Seq[Move]
  ): Unit = {
    executeMovesPartTwo(stacks, moves)

    val tops = stacks.map(_.top).mkString
    println(s"PART TWO: Message displayed by all crates at the top: $tops")
  }

  private def executeMovesPartTwo(
      stacks: Seq[mutable.Stack[Char]],
      moves: Seq[Move]
  ): Unit = {
    moves
      .foldLeft(stacks) { case (accStacks, Move(qty, origin, dest)) =>
        val popped = (0 to qty - 1).map(_ => accStacks(origin - 1).pop)
        accStacks(dest - 1).pushAll(popped.reverse)

        accStacks
      }
  }

  private def executeMovesPartOne(
      stacks: Seq[mutable.Stack[Char]],
      moves: Seq[Move]
  ): Seq[mutable.Stack[Char]] = {
    moves
      .flatMap { move =>
        (0 to move.qty - 1).map(_ => Tuple2(move.origin, move.dest))
      }
      .foldLeft(stacks) { case (accStacks, (origin, dest)) =>
        val popped = accStacks(origin - 1).pop()
        accStacks(dest - 1).push(popped)

        accStacks
      }
  }

  private def processMoveLines(moveLines: Seq[String]): Seq[Move] = {
    moveLines.map { line =>
      val nums = ("\\d+".r).findAllIn(line).map(_.toInt).toSeq
      Move(nums(0), nums(1), nums(2))
    }
  }

  private def processLayoutLines(
      layoutLines: Seq[String]
  ): Seq[mutable.Stack[Char]] = {
    val lineLength = layoutLines(0).length
    val widthInCrates: Int = (lineLength + 1) / 4
    val numberOfShelves = layoutLines.length
    val allCrates = layoutLines.mkString

    (0 to widthInCrates - 1).foldLeft(Seq.empty[mutable.Stack[Char]]) {
      case (acc, stackIndex) => {
        val charsInStack = (0 to numberOfShelves - 1).map { shelfIndex =>
          val idx = (shelfIndex * lineLength) + (stackIndex * 4) + 1
          allCrates.charAt(idx) match {
            case c if c.isWhitespace => None
            case c                   => Some(c)
          }
        }
        acc :+ mutable.Stack.from(charsInStack.flatten)
      }
    }
  }

  case class Move(qty: Int, origin: Int, dest: Int)
}

/*
     [D]    [N] [C]    [Z] [M] [P]
 */
