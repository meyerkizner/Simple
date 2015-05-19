package com.prealpha.simple

abstract sealed class BinaryHeap[T: Ordering] extends SimpleQueue[T] {
  override def enqueue(elem: T): BinaryHeap[T]

  override def dequeue: BinaryHeap[T]

  protected def size: Int

  protected def height: Int

  protected def extractLast: (Option[T], BinaryHeap[T])

  protected def heapify(newValue: T): BinaryHeap[T]
}

object BinaryHeap {
  private case class Empty[T: Ordering]() extends BinaryHeap[T] {
    override def enqueue(elem: T): BinaryHeap[T] = Node(this, elem, this)

    override val dequeue: BinaryHeap[T] = this

    override val peek: Option[T] = None

    override protected val size = 0

    override protected val height = 0 // different from the standard def'n, to make things work out

    override protected val extractLast: (Option[T], BinaryHeap[T]) = (None, this)

    override protected def heapify(newValue: T): BinaryHeap[T] = Node(this, newValue, this)
  }

  private case class Node[T: Ordering](left: BinaryHeap[T], value: T, right: BinaryHeap[T]) extends BinaryHeap[T] {
    import scala.math.Ordering.Implicits._

    override def enqueue(elem: T): BinaryHeap[T] = {
      if (elem != value) {
        // if elem < value, elem should be the root and we insert value into a child
        val toInsert = if (elem < value) value else elem
        val newRoot = if (elem < value) elem else value
        // to make sure we get a valid binary heap, insert into the leftmost imperfect child
        // if both children are perfect, then the shorter one; if both the same height, the left child
        if (left.size < Math.pow(2, left.height) - 1) {
          Node(left.enqueue(toInsert), newRoot, right)
        } else if (right.size < Math.pow(2, right.height) - 1) {
          Node(left, newRoot, right.enqueue(toInsert))
        } else if (right.height < left.height) {
          Node(left, newRoot, right.enqueue(toInsert))
        } else {
          Node(left.enqueue(toInsert), newRoot, right)
        }
      } else {
        this
      }
    }

    override def dequeue: BinaryHeap[T] = extractLast match {
      case (None, _) => Empty()
      case (Some(last), heap) => heap.heapify(last)
    }

    override val peek: Option[T] = Some(value)

    override protected lazy val size = left.size + right.size + 1

    override protected lazy val height = Math.max(left.height, right.height) + 1

    override protected def extractLast: (Option[T], BinaryHeap[T]) = {
      if (left.height > right.height) {
        left.extractLast match {
          case (None, _) => (Some(value), Empty())
          case (Some(last), leftChild) => (Some(last), Node(leftChild, value, right))
        }
      } else {
        right.extractLast match {
          case (None, _) => (Some(value), Empty())
          case (Some(last), rightChild) => (Some(last), Node(left, value, rightChild))
        }
      }
    }

    override protected[simple] def heapify(newValue: T): BinaryHeap[T] = (left.peek, right.peek) match {
      case (None, None) => Node(left, newValue, right)
      case (Some(leftMin), None) =>
        if (leftMin < newValue) {
          Node(left.heapify(newValue), leftMin, right)
        } else {
          Node(left, newValue, right)
        }
      case (None, Some(rightMin)) =>
        if (rightMin < newValue) {
          Node(left, rightMin, right.heapify(newValue))
        } else {
          Node(left, newValue, right)
        }
      case (Some(leftMin), Some(rightMin)) =>
        if (leftMin < rightMin && leftMin < newValue) {
          Node(left.heapify(newValue), leftMin, right)
        } else if (rightMin < leftMin && rightMin < newValue) {
          Node(left, rightMin, right.heapify(newValue))
        } else {
          Node(left, newValue, right)
        }
    }
  }

  def apply[T: Ordering](xs: T*): BinaryHeap[T] = {
    val completeHeight = Math.floor(Math.log(xs.size + 1) / Math.log(2)).toInt
    val completeSize = Math.pow(2, completeHeight).toInt - 1
    val (completeValues, leafValues) = xs.splitAt(completeSize)
    val leaves = (leafValues map Empty().enqueue) ++ Seq.fill(Math.pow(2, completeHeight + 1).toInt - xs.size - 1)(Empty())
    ((completeValues, leaves.toIterator) /: (completeHeight to 1 by -1)) { (state, i) =>
      val (values, heaps) = state
      val pairedHeaps = heaps.grouped(2)
      val (used, remainder) = values.splitAt(Math.pow(2, i - 1).toInt)
      val newHeaps = used.toIterator.zip(pairedHeaps) map {
        case (value, Seq(left, right)) => Node(left, value, right).heapify(value)
      }
      (remainder, newHeaps)
    }._2.next()
  }
}
