package main
import scala.util.Using
import scala.io.Source
import scala.util.Failure.apply
import scala.util.Try
import scala.util.Failure
import scala.util.Success
import puzzles.*

trait Puzzle {
  def run(filepath: String): Unit
}

case class PuzzleError(message: String)

object Puzzles {
  val knownPuzzles: Seq[Puzzle] = Array(
    PuzzleOne,
    PuzzleTwo,
    PuzzleThree,
    PuzzleFour,
    PuzzleFive
  )
}

@main def main(puzzleTarget: String, otherPuzzleTargets: Int*): Unit = {
  // Main target can be '*' to run ALL puzzles,
  // or an integer to be run normally.
  if (puzzleTarget == "*") {
    // Run ALL puzzles
    runPuzzles(Puzzles.knownPuzzles)
  } else {
    Try(puzzleTarget.toInt) match {
      case Failure(exception) =>
        println(
          s"ERR! Puzzle target $puzzleTarget not a number, nor is it the '*' wildcard to run all puzzles"
        )
      case Success(value) =>
        val puzzles =
          (value +: otherPuzzleTargets)
            .map(i => i - 1)
            .distinct
            .map(Puzzles.knownPuzzles.apply)
        runPuzzles(puzzles)
    }
  }
}

private def runPuzzles(puzzles: Seq[Puzzle]): Unit = {
  puzzles.foreach { puzzle =>
    val index = Puzzles.knownPuzzles.indexOf(puzzle)
    if (index == -1) {
      throw new IllegalStateException(
        "Puzzle provided is not known to our list of known puzzles"
      )
    }

    // Using(Source.fromFile(s"resources/puzzle_$index.dat")) { source =>

    // }
    println(s"== EXECUTING PUZZLE ${index + 1} ==\n")
    puzzle.run(s"resources/puzzle_${index + 1}.dat")
    println(s"== PUZZLE ${index + 1} EXECUTION FINISH ==\n")
  }
}
