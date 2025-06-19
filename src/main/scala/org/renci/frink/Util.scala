package org.renci.frink

object Util:

  trait SizedIterator[+A]:

    def iterator: Iterator[A]

    def size: BigInt

    def drop(num: BigInt): SizedIterator[A]

    def map[B](f: A => B): SizedIterator[B]

  object SizedIterator:
    val empty: SizedIterator[Nothing] = SingleSizedIterator(Iterator.empty, 0)

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
      if num > this.size then SizedIterator.empty
      else
        var remainingToDrop = num
        var remainingIterators = iterators
        while (remainingToDrop > remainingIterators.head.size)
          remainingToDrop = remainingToDrop - remainingIterators.head.size
          remainingIterators = remainingIterators.drop(1)
        MultiSizedIterator(remainingIterators.updated(0, remainingIterators.head.drop(remainingToDrop)))

    def map[B](f: A => B): SizedIterator[B] = MultiSizedIterator(iterators.map(_.map(f)))
