package puzzles

import scala.io.Source
import scala.util.Try
import scala.util.Failure.apply
import scala.util.Using
import scala.util.Success
import scala.util.Failure
import main.Puzzle

object PuzzleOne extends Puzzle {
  override def partOne(lines: Seq[String]): Unit = {
    val summedBlocks = sumDataBlocks(extractCalories(lines)) match {
      case Left(err) => println(s"ERR! ${err.toString}")
      case Right(value) => {
        val topThreeMax = value
          .sorted(Ordering.Int.reverse)
          .take(3)

        println(
          s"Calories carried by top-three highest carriers = ${topThreeMax.sum} (top-three values = ${topThreeMax})"
        )
      }
    }
  }
  override def partTwo(lines: Seq[String]): Unit = {
    val summedBlocks = sumDataBlocks(extractCalories(lines)) match {
      case Left(err) => println(s"ERR! ${err.toString}")
      case Right(value) => {
        println(
          s"Calories carried by highest-calorie-carrying elf = ${value.max}"
        )
      }
    }
  }

  private def extractCalories(
      lines: Seq[String]
  ): Seq[DataBlock] = {
    lines.foldLeft(Seq(DataBlock(Seq.empty))) {
      case ((headAccBlocks :+ lastAccBlock), newLine) =>
        if (newLine.isEmpty()) {
          // End of block, "tie off" current data block by appending a new one
          headAccBlocks ++ Seq(lastAccBlock, DataBlock(Seq.empty))
        } else {
          // Continuation of block, append new line to working block
          headAccBlocks :+ DataBlock(lastAccBlock.dataLines :+ newLine)
        }
    }
  }

  private def sumDataBlocks(
      dataBlocks: Seq[DataBlock]
  ): Either[PuzzleError, Seq[Int]] =
    dataBlocks.map(sumDataBlock).partitionMap(identity) match {
      case (Nil, ints)          => Right(ints)
      case (firstError :: _, _) => Left(firstError)
    }

  private def sumDataBlock(
      dataBlock: DataBlock
  ): Either[PuzzleError, Int] = {
    dataBlock.dataLines
      .map { strNum =>
        Try(Integer.parseInt(strNum)) match {
          case Success(value) => Right(value)
          case Failure(exception) =>
            Left(
              PuzzleError(
                s"Malformed line '$strNum' -> ${exception.getMessage()} (cannot parseInt)"
              )
            )
        }
      }
      .partitionMap(identity) match {
      case (Nil, ints)          => Right(ints.sum)
      case (firstError :: _, _) => Left(firstError)
    }
  }

  case class DataBlock(dataLines: Seq[String])

  case class PuzzleError(message: String)
}
