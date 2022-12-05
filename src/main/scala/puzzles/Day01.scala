package puzzles

import scala.io.Source
import scala.util.Try
import scala.util.Failure.apply
import scala.util.Using
import scala.util.Success
import scala.util.Failure
import main.Puzzle

/** Advent of Code 2022 - Puzzle One
  *
  * Given a file which has multiple blocks of newlines, seperated each by TWO
  * consecutive newlines, this program must calculate the index of the largest
  * block (where the size of the block is calculated by summing all the numbers
  * in each block).
  */

object PuzzleOne extends Puzzle {
  override def run(filepath: String): Unit = {
    val eitherT = for {
      blocks <- loadInputFromFile(filepath)
      summedBlocks <- sumDataBlocks(blocks)
    } yield summedBlocks

    eitherT match {
      case Left(err) => println(s"ERR! ${err.toString}")
      case Right(value) => {
        val topThreeMax = value
          .sorted(Ordering.Int.reverse)
          .take(3)

        println(
          s"Calories carried by highest-calorie-carrying elf = ${topThreeMax(0)}"
        )
        println(
          s"Calories carried by top-three highest carriers = ${topThreeMax.sum} (top-three values = ${topThreeMax})"
        )
      }
    }
  }

  /** Given a filepath, this method opens the file at that location (if any) and
    * loads the data, providing an array of "blocks" that are defined as being
    * seperated by two consecutive newlines (i.e. a blank line)
    *
    * @param filepath
    *   The filepath to load
    * @return
    *   The data blocks from the file
    */
  private def loadInputFromFile(
      filepath: String
  ): Either[PuzzleError, Seq[DataBlock]] = {
    Using(Source.fromFile(filepath)) { case source =>
      source
        .getLines()
        .foldLeft(Seq(DataBlock(Seq.empty))) {
          case ((headAccBlocks :+ lastAccBlock), newLine) =>
            if (newLine.isEmpty()) {
              // End of block, "tie off" current data block by appending a new one
              headAccBlocks ++ Seq(lastAccBlock, DataBlock(Seq.empty))
            } else {
              // Continuation of block, append new line to working block
              headAccBlocks :+ DataBlock(lastAccBlock.dataLines :+ newLine)
            }
        }
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

  /** Given a list of [[DataBlock]]s, this method will calculate the sum of each
    *
    * @param dataBlocks
    * @return
    *   An Either containing
    *   - Right containing the summed value of the data block if success
    *   - Left containing a [[PuzzleError]] if the parsing of the datalines
    *     inside a data block failed
    */
  private def sumDataBlocks(
      dataBlocks: Seq[DataBlock]
  ): Either[PuzzleError, Seq[Int]] =
    dataBlocks.map(sumDataBlock).partitionMap(identity) match {
      case (Nil, ints)          => Right(ints)
      case (firstError :: _, _) => Left(firstError)
    }

  /** Given a data block this method will attempt to parse all it's contained
    * data lines in to an Int
    *
    * @param dataBlock
    * @return
    *   An either containing:
    *   - Right containing the summed value of this data block on success
    *   - Left containing a [[PuzzleError]] if one of the data lines in this
    *     block could not be parsed
    */
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
}

case class DataBlock(dataLines: Seq[String])

case class PuzzleError(message: String)
