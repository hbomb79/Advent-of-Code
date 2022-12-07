package puzzles

import main.Puzzle
import scala.annotation.tailrec

object PuzzleSeven extends Puzzle {
  override def partOne(fileLines: Seq[String]): Unit = {
    println(
      findDirSizes
        .tupled(parseLines(fileLines))
        .map(_._2)
        .filter(_ <= 100_000)
        .sum
    )
  }
  override def partTwo(fileLines: Seq[String]): Unit = {
    val dirSizes = findDirSizes.tupled(parseLines(fileLines))
    val spaceToBeFreed = 30000000 - (70000000 - dirSizes.map(_._2).max)
    println(dirSizes.filter((_, size) => size >= spaceToBeFreed).minBy(_._2))
  }

  /** Given a list of [[File]] instances, and a Set of all the [[Dir]] we
    * encountered during parsing, this method will return the sizes for all the
    * directories (where the size includes the the size of all the
    * sub-directories).
    *
    * @param files
    *   The files to process, where each file contains it's parent path
    *   [[File.parent]]
    * @param allDirs
    *   All dirs we wish to know the size of
    * @return
    *   A list of 2-element tuples, containing the directory and it's size
    */
  private def findDirSizes(
      files: List[File],
      allDirs: Set[Dir]
  ): List[(Dir, Long)] = {
    allDirs.toList.map(dirStack =>
      dirStack -> files
        .filter(_.parent.path.startsWith(dirStack.path))
        .map(_.size)
        .sum
    )
  }

  private def parseLines(lines: Seq[String]): (List[File], Set[Dir]) =
    parseLines(lines, List.empty, Dir(List.empty), Set.empty)

  /** Recursively consumes the lines provided until the end of the list. For
    * each recursion, will try to extract one of:
    *   - a `$ cd ..` line: causing dirStack to be popped,
    *   - a `$ cd NAME` line: cause a dir of name `NAME` to be pushed on to the
    *     dir stack
    *   - an `$ ls` line: passes lines to [[parseListLines]] and appends the new
    *     files found
    *
    * @param lines
    *   The lines left to process
    * @param files
    *   The files discovered so far
    * @param cwd
    *   The current working directory, represented as the path to this dir in
    *   stack/List form
    * @param allDirs
    *   All directories we've CD'd in to so far
    * @return
    */
  @tailrec
  private def parseLines(
      lines: Seq[String],
      files: List[File],
      cwd: Dir,
      allDirs: Set[Dir]
  ): (List[File], Set[Dir]) = {
    lines match {
      case Nil => (files, allDirs)
      case "$ cd .." :: next =>
        parseLines(next, files, cwd.drop, allDirs)
      case s"$$ cd $name" :: next =>
        val newDir = cwd.append(name)
        parseLines(next, files, newDir, allDirs + newDir)
      case "$ ls" :: next => {
        val (rest, listFiles) = parseListLines(next, cwd)
        parseLines(rest, files ++ listFiles, cwd, allDirs)
      }
    }
  }

  /** Given some lines (starting with those that have been generated by the
    * output of `ls`), this method will consume only those lines and find all
    * the new [[File]]s. 'dir' lines are ignored, and parsing will stop when:
    *   - The input terminates
    *   - A line of the input is not from the output of an `ls` command (e.g. a
    *     command, starting with $)
    *
    * @param lines
    *   The lines to parse, starting with lines generated by `ls`
    * @param cwd
    *   The current directory from where this `ls` output originated from which
    *   is the [[parent]] key used when instantiating [[File]] instances
    * @return
    *   A tuple containing two elements:
    *   - The lines from the provided input that were NOT consumed by this
    *     parsing
    *   - The newly discovered files from this parse
    */
  private def parseListLines(
      lines: Seq[String],
      cwd: Dir
  ): (Seq[String], Seq[File]) = {
    val lsLines = lines.takeWhile(!_.startsWith("$"))
    val files = lsLines.foldLeft(Seq.empty[File]) {
      case (acc, s) if s.startsWith("dir") =>
        acc
      case (acc, s"$size $name") =>
        acc :+ File(name, size.toLong, cwd)
    }

    (lines.drop(lsLines.length), files)
  }

  case class File(name: String, size: Long, parent: Dir)
  case class Dir(path: List[String]) {
    def drop: Dir = Dir(this.path.dropRight(1))
    def append(segment: String): Dir = Dir(this.path :+ segment)
  }
}
