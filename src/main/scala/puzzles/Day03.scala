package puzzles

import scala.io.Source
import scala.util.Try
import scala.util.Failure.apply
import scala.util.Using
import scala.util.Success
import scala.util.Failure
import main.Puzzle

object PuzzleThree extends Puzzle {
  override def partOne(lines: Seq[String]): Unit = {
    val rucksacks = extractRucksacks(lines)
    val duplicates = findDuplicatesItemsInEachRucksack(rucksacks).flatten
    println(s"Sum of duplicate item priorities: ${duplicates.sum}")
  }

  override def partTwo(lines: Seq[String]): Unit = {
    val rucksacks = extractRucksacks(lines)
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

  private def extractRucksacks(
      lines: Seq[String]
  ): Seq[Rucksack] =
    lines
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
}
