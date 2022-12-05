package puzzles

import scala.io.Source
import scala.util.Try
import scala.util.Failure.apply
import scala.util.Using
import scala.util.Success
import scala.util.Failure
import main.Puzzle

/** Advent of Code 2022 - Puzzle Three
  *
  * Given a file which has a multiple lists of letters, where each list
  * represents the concatentated contents of two compartments for each rucksack,
  * this program must count and classify the contents of each compartment and
  * find and count the duplicated items in each.
  */

object PuzzleThree extends Puzzle {
  override def run(filepath: String): Unit = {
    loadInputFromFile(filepath) match {
      case Left(err) => println(s"ERR! ${err.toString()}")
      case Right(rucksacks) =>
        val duplicates = findDuplicatesItemsInEachRucksack(rucksacks)
        val identities = findIdentifyingItemItemsForGroups(rucksacks).toSeq
        identities.flatten match {
          case flattened if flattened.length != identities.length =>
            println(
              s"ERR! One or more group identities could not be calculated (len diff ${flattened.length} != ${identities.length}"
            )
          case flattened =>
            println(
              s"Flattened sum of group identity item priorities: ${flattened.sum}"
            )
        }

    }
  }

  private def findDuplicatesItemsInEachRucksack(
      rucksacks: Seq[Rucksack]
  ): Seq[Seq[Int]] = {
    rucksacks.map { case Rucksack(Compartment(compA), Compartment(compB)) =>
      // Collect all elements from compOne that exist in compTwo
      compA.filter(compB.contains).distinct
    }
  }

  private def findIdentifyingItemItemsForGroups(
      rucksacks: Seq[Rucksack]
  ): Iterator[Option[Int]] = {
    rucksacks.grouped(3).map { groupRucksacks =>
      // Take a distinct view of each rucksack - we do not care about the compartments
      // and wish to ignore any duplicates within the same rucksack
      val combinedRucksacks = groupRucksacks.map(r =>
        (r.compOne.itemPriorities ++ r.compTwo.itemPriorities).distinct
      )

      // Folding over each rucksack, keep track of every time we see a certain item
      // priority. At the end the one we have three of is the answer to the identity of this group
      val itemsCounted = combinedRucksacks.foldLeft(Map.empty[Int, Int]) {
        case (record, nextRucksack) =>
          val newEntries = nextRucksack.map { item =>
            val existing = record.getOrElse(item, 0)
            item -> (existing + 1)
          }.toMap

          record ++ newEntries
      }

      itemsCounted.find { case (_, count) => count == 3 }.map(_._1)
    }
  }

  /** Given a filepath, this method opens the file at that location (if any) and
    * loads the data by reading each line, finding the priority for each item on
    * each line, and then splitting the line in half in order to construct a
    * [[Rucksack]] with two equally sized [[Compartment]]s
    *
    * @param filepath
    *   The filepath to load
    * @return
    *   The rucksacks from the file
    */
  private def loadInputFromFile(
      filepath: String
  ): Either[PuzzleError, Seq[Rucksack]] = {
    Using(Source.fromFile(filepath)) { case source =>
      source
        .getLines()
        .map(line =>
          line.map(char =>
            Item.getPriority(char) match {
              case Some(priority) => priority
              case None =>
                throw new IllegalArgumentException(
                  s"Invalid char '$char' found in line $line"
                )
            }
          )
        )
        .map { priorities =>
          val (compA, compB) = priorities.splitAt(priorities.length / 2)
          Rucksack(
            Compartment(compA),
            Compartment(compB)
          )
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
}

case class Compartment(itemPriorities: Seq[Int])
case class Rucksack(compOne: Compartment, compTwo: Compartment)

object Item {
  private final val BaseUppercasePriority = 27
  private final val BaseLowercasePriority = 1

  def getPriority(char: Char): Option[Int] = {
    char.toInt match {
      case c if c >= 'A'.toInt && c.toInt <= 'Z'.toInt =>
        val index = c - 'A'.toInt
        Some(index + BaseUppercasePriority)
      case c if c >= 'a'.toInt && c.toInt <= 'z'.toInt =>
        val index = c - 'a'.toInt
        Some(index + BaseLowercasePriority)
      case _ => None
    }
  }
}
