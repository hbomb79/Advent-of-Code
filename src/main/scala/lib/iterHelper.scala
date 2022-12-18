import Ordering.Implicits._
import scala.collection.mutable.{ArrayBuffer, Map}
package object IterHelper {

  /** Similar to takeWhile, except this counts the number of times a take was
    * successful *and* will still count the element that fails the limit test.
    * Hence names 'until' as it implies it will take at least one item (if any
    * available) even if >= limit
    *
    * @param items
    *   Items to count from
    * @param limit
    *   Items must not exceed this limit, if one is found that does, the
    *   counting stops (after incrementing to account for this item)
    * @return
    *   The total number of items that were counted
    */
  def countUntil[T: Ordering](
      items: Seq[T],
      limit: T
  ): Int = items match {
    case Nil                        => 0
    case head +: _ if head >= limit => 1
    case head +: next               => 1 + countUntil(next, limit)
  }

  /** Given a list, returns all the combinations where each element is
    * extracted, and the remaining elements are returned alongside E.g. for
    * input [1,2,3,4,5] you'd receive:
    *   - (1, [2,3,4,5]),
    *   - (2, [1,3,4,5]),
    *   - (3, [1,2,4,5]),
    *   - ...
    */
  def elementExtractCombinations[A](
      list: List[A]
  ): Iterable[(A, List[A])] =
    list.indices.map { i =>
      val split = list.splitAt(i)
      (list(i), split._1 ::: split._2.drop(1))
    }

  /** Returns shortest paths between all pairs using Floyd-Warshall algorithm.
    * Nodes are assumed to be enumerated without any holes and enumeration
    * starts from 0.
    *
    * @param nodes
    *   the set of vertices
    * @param links
    *   the map of edges with costs
    * @return
    *   shortest paths between all pairs, including the source and destination
    */
  def allPairsShortestPath(
      nodes: Set[Int],
      links: Map[Int, Set[Int]]
  ): Map[Int, Map[Int, Seq[Int]]] = {
    val n = nodes.size
    val inf = Int.MaxValue

    // Initialize distance matrix.
    val ds = Array.fill[Int](n, n)(inf)
    for (i <- 0 until n) ds(i)(i) = 0
    for (i <- links.keys)
      for (j <- links(i))
        ds(i)(j) = 1

    // Initialize next vertex matrix.
    val ns = Array.fill[Int](n, n)(-1)

    // Here goes the magic!
    for (k <- 0 until n; i <- 0 until n; j <- 0 until n)
      if (
        ds(i)(k) != inf && ds(k)(j) != inf && ds(i)(k) + ds(k)(j) < ds(i)(j)
      ) {
        ds(i)(j) = ds(i)(k) + ds(k)(j)
        ns(i)(j) = k
      }

    // Helper function to carve out paths from the next vertex matrix.
    def extractPath(path: ArrayBuffer[Int], i: Int, j: Int): Unit = {
      if (ds(i)(j) == inf) return
      val k = ns(i)(j)
      if (k != -1) {
        extractPath(path, i, k)
        path.append(k)
        extractPath(path, k, j)
      }
    }

    // Extract paths.
    val pss = Map[Int, Map[Int, Seq[Int]]]()
    for (i <- 0 until n) {
      val ps = Map[Int, Seq[Int]]()
      for (j <- 0 until n)
        if (ds(i)(j) != inf) {
          val p = new ArrayBuffer[Int]()
          p.append(i)
          if (i != j) {
            extractPath(p, i, j)
            p.append(j)
          }
          ps(j) = p.toSeq
        }
      pss(i) = Map.from(ps)
    }

    // Return extracted paths.
    Map.from(pss)
  }
}
