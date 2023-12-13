package puzzles

import scala.io.Source
import scala.util.Try
import scala.util.Failure.apply
import scala.util.Using
import scala.util.Success
import scala.util.Failure
import main.Puzzle

object PuzzleTwo extends Puzzle {
  override def partOne(lines: Seq[String]): Unit = {
    val moves = extractPlayerMoves(lines)
    println(s"Part one Score: ${calculatePartOneScores(moves)}")
  }

  override def partTwo(lines: Seq[String]): Unit = {
    val moves = extractPlayerMoves(lines)
    println(s"Part two Score: ${calculatePartTwoScores(moves)}")
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

  private def extractPlayerMoves(
      lines: Seq[String]
  ): Seq[PlayerMoves] = {
    lines.map { case line =>
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
    }.toSeq
  }

  private def getMoveForChar(char: Int): Option[Move] = {
    char match {
      case 'A' | 'X' => Some(Rock)
      case 'B' | 'Y' => Some(Paper)
      case 'C' | 'Z' => Some(Scissors)
      case _         => None
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
}
