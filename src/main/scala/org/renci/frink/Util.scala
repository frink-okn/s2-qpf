package org.renci.frink

object Util:

  trait SizedIterator[+A]:

    def iterator: Iterator[A]

    def size: BigInt

    def drop(num: BigInt): SizedIterator[A]

    def map[B](f: A => B): SizedIterator[B]

  object SizedIterator:
    val empty: SizedIterator[Nothing] = SingleSizedIterator(Iterator.empty, 0)

    def fromSeq[A](items: Seq[A]): SizedIterator[A] = SingleSizedIterator(items.iterator, items.size)

  final class SingleSizedIterator[+A](val iterator: Iterator[A], val size: BigInt) extends SizedIterator[A]:

    def drop(num: BigInt): SizedIterator[A] =
      if num > this.size then SizedIterator.empty
      else
        var remaining = num
        while (remaining > 0 && iterator.hasNext)
          iterator.next()
          remaining -= 1
        SingleSizedIterator(iterator, size - num)

    def map[B](f: A => B): SizedIterator[B] = SingleSizedIterator(iterator.map[B](f), size)

  final class MultiSizedIterator[+A](iterators: Vector[SizedIterator[A]]) extends SizedIterator[A]:

    def iterator: Iterator[A] = iterators.iterator.map(_.iterator).flatten

    def size: BigInt = iterators.map(_.size).sum

    def drop(num: BigInt): SizedIterator[A] =
      if num >= this.size then SizedIterator.empty
      else
        var remainingToDrop = num
        var remainingIterators = iterators
        while (remainingToDrop > remainingIterators.head.size)
          remainingToDrop = remainingToDrop - remainingIterators.head.size
          remainingIterators = remainingIterators.drop(1)
        MultiSizedIterator(remainingIterators.updated(0, remainingIterators.head.drop(remainingToDrop)))

    def map[B](f: A => B): SizedIterator[B] = MultiSizedIterator(iterators.map(_.map(f)))

  /** Expands every element of `source` into exactly `fanOut` elements, so that dropping can skip source elements without expanding them.
    */
  final class FanOutIterator[A, +B](source: SizedIterator[A], fanOut: Int, expand: A => Seq[B], offset: Int = 0) extends SizedIterator[B]:
    require(fanOut > 0)

    def iterator: Iterator[B] =
      source.iterator
        .flatMap { item =>
          val expanded = expand(item)
          require(expanded.size == fanOut, s"Expected $fanOut elements but got ${expanded.size}")
          expanded
        }
        .drop(offset)

    def size: BigInt = source.size * fanOut - offset

    def drop(num: BigInt): SizedIterator[B] =
      if num >= this.size then SizedIterator.empty
      else
        val (sourceToDrop, remainder) = (offset + num) /% fanOut
        FanOutIterator(source.drop(sourceToDrop), fanOut, expand, remainder.toInt)

    def map[C](f: B => C): SizedIterator[C] = FanOutIterator(source, fanOut, expand.andThen(_.map(f)), offset)
