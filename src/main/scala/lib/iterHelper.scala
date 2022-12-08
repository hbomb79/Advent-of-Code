import Ordering.Implicits._
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
}
