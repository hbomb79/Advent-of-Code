package puzzles

import scala.io.Source
import scala.util.Try
import scala.util.Failure.apply
import scala.util.Using
import scala.util.Success
import scala.util.Failure
import main.Puzzle

/** Advent of Code 2022 - Puzzle Two
  *
  * Given a file which has a list of Rock-Paper-Scissor moves (your opponenets
  * and your own), this program will calculate your exact score assuming the
  * game is played exactly as is dictated by the list.
  *
  * Game has two parts, first part we (wrongly) assumed the right side of the
  * input were the moves we were supposed to make. The first part
  * (calculatePartOneScores) returns the score using this strategy. The actual
  * meaning behind the right column is that it dictates the OUTCOME of the game,
  * so calculatePartTwoScores adjusts the loaded moves so that the moves result
  * in the outcome requires (Win/Loss/Draw)
  */

object PuzzleTwo extends Puzzle {
  override def run(filepath: String): Unit = {
    loadInputFromFile(filepath) match {
      case Left(err) => println(s"ERR! ${err.toString()}")
      case Right(moves) =>
        println(s"Part one Score: ${calculatePartOneScores(moves)}")
        println(s"Part two Score: ${calculatePartTwoScores(moves)}")
    }
  }

  private def calculatePartOneScores(moves: Seq[PlayerMoves]): Int = {
    moves.foldLeft(0) { case (acc, next) =>
      val baseScore = next.yourMove.score
      val additionalScore = next match {
        case PlayerMoves(opMove, yourMove) if opMove == yourMove =>
          // Draw
          3
        case move @ PlayerMoves(opMove, yourMove) =>
          if (PlayerMoves.isPlayerWinner(move)) {
            // Win
            6
          } else {
            // Lose
            0
          }
      }

      acc + baseScore + additionalScore
    }
  }

  private def calculatePartTwoScores(moves: Seq[PlayerMoves]): Int = {
    val newMoves = moves.map { case PlayerMoves(opMove, yourMove) =>
      yourMove match {
        // X = needs to lose
        case Rock => PlayerMoves(opMove, opMove.getWinMove)

        // Y = needs to draw
        case Paper => PlayerMoves(opMove, opMove.getDrawMove)

        // Z = needs to win
        case Scissors => PlayerMoves(opMove, opMove.getLoseMove)
      }
    }
    calculatePartOneScores(newMoves)
  }

  /** Given a filepath, this method opens the file at that location (if any) and
    * loads the data, providing an array of "moves"
    *
    * @param filepath
    *   The filepath to load
    * @return
    *   The moves from the file
    */
  private def loadInputFromFile(
      filepath: String
  ): Either[PuzzleError, Seq[PlayerMoves]] = {
    Using(Source.fromFile(filepath)) { case source =>
      source
        .getLines()
        .map { case line =>
          if (line.length == 3) {
            // Extract first and third char
            val chars = line.chars().toArray()
            (getMoveForChar(chars(0)), getMoveForChar(chars(2))) match {
              case (None, _) | (_, None) =>
                throw new IllegalArgumentException(
                  s"File data is invalid, line $line contains unknown moves"
                )
              case (Some(hostileMove), Some(playerMove)) =>
                PlayerMoves(hostileMove, playerMove)
            }
          } else {
            throw new IllegalArgumentException(
              "File data is invalid, all lines must be 3 characters long containing two moves seperated by one whitespace"
            )
          }
        }
        .toSeq
    }.toEither match {
      case Left(throwable) =>
        Left(
          PuzzleError(
            s"Failed to construct source with filepath $filepath: ${throwable.getMessage()}"
          )
        )
      case Right(value) => Right(value)
    }
  }

  private def getMoveForChar(char: Int): Option[Move] = {
    char match {
      case 'A' | 'X' => Some(Rock)
      case 'B' | 'Y' => Some(Paper)
      case 'C' | 'Z' => Some(Scissors)
      case _         => None
    }
  }
}

sealed trait Move {
  val score = 0
  def getDrawMove: Move = this
  def getLoseMove: Move = this
  def getWinMove: Move = this
}
case object Rock extends Move {
  override val score = 1

  override def getLoseMove: Move = Paper
  override def getWinMove: Move = Scissors
}
case object Paper extends Move {
  override val score = 2
  override def getLoseMove: Move = Scissors
  override def getWinMove: Move = Rock
}
case object Scissors extends Move {
  override val score = 3
  override def getLoseMove: Move = Rock
  override def getWinMove: Move = Paper
}

case class PlayerMoves(opMove: Move, yourMove: Move)
object PlayerMoves {
  def isPlayerWinner(move: PlayerMoves): Boolean = {
    move match {
      case PlayerMoves(Scissors, Rock)  => true
      case PlayerMoves(Paper, Scissors) => true
      case PlayerMoves(Rock, Paper)     => true
      case _                            => false
    }
  }
}
