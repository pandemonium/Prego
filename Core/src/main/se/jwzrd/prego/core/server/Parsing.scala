package se.jwzrd.prego.core.server

/**
 * @author Patrik Andersson <pandersson@gmail.com>
 */
trait Parsing {
  type ParseResult = String Either String

  def parseUntil(source: Iterator[Char])(delimitor: Seq[Char]): ParseResult = {
    if (!delimitor.isEmpty) {
      val head = delimitor head
      val taken = source takeWhile (head !=) toArray

      if (prefixMatches(source)(delimitor tail))
        emit(taken)
      else
        expected(delimitor)
    } else
      emit(source toArray)
  }

  def emit(source: Array[Char]) =
    Right(new String(source toArray))

  def expected(what: Seq[Char]) =
    Left("expected [" + what + "]")

  def prefixMatches(source: Iterator[Char])(limit: Seq[Char]): Boolean =
    if (source.hasNext && !limit.isEmpty) {
      val tail = limit.tail
      limit.head == source.next && (tail.isEmpty || prefixMatches(source)(tail))
    } else
      limit.isEmpty
}