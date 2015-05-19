package com.prealpha.simple

final class BinomialHeap[T: Ordering] private (private val trees: List[BinomialHeap.Node[T]]) extends SimpleQueue[T] {
  import scala.math.Ordering.Implicits._

  private def this(l1: List[BinomialHeap.Node[T]], l2: List[BinomialHeap.Node[T]]) = {
    this(BinomialHeap.mergeTrees(l1, l2, None))
  }

  override def enqueue(elem: T): BinomialHeap[T] = {
    val newTrees = List(BinomialHeap.Node(elem, IndexedSeq.empty))
    new BinomialHeap(trees, newTrees)
  }

  override def dequeue: BinomialHeap[T] = {
    val tree = trees.reduceOption { (t1, t2) =>
      if (t1.value < t2.value) t1 else t2
    }
    tree match {
      case None => this
      case Some(removedTree) =>
        val otherTrees = trees.filterNot(removedTree.eq)
        new BinomialHeap(otherTrees, removedTree.children.toList)
    }
  }

  override val peek = trees.map(_.value).reduceOption(implicitly[Ordering[T]].min)
}

object BinomialHeap {
  private case class Node[T: Ordering](value: T, children: IndexedSeq[Node[T]]) {
    import scala.math.Ordering.Implicits._

    private[BinomialHeap] def merge(other: Node[T]): Node[T] = {
      if (value < other.value) {
        Node(value, children :+ other)
      } else {
        Node(other.value, other.children :+ this)
      }
    }
  }

  private def mergeTrees[T: Ordering](l1: List[Node[T]], l2: List[Node[T]], carry: Option[Node[T]]): List[Node[T]] = (l1, l2, carry) match {
    case (_, Nil, None) => l1
    case (_, Nil, Some(h3)) => mergeTrees(l1, List(h3), None)
    case (Nil, _, None) => l2
    case (Nil, _, Some(h3)) => mergeTrees(List(h3), l2, None)
    case (h1 :: t1, h2 :: t2, None) =>
      if (h1.children.length == h2.children.length) {
        mergeTrees(t1, t2, Some(h1.merge(h2)))
      } else if (h1.children.length < h2.children.length) {
        h1 :: mergeTrees(t1, l2, None)
      } else {
        h2 :: mergeTrees(l1, t2, None)
      }
    case (h1 :: t1, h2 :: t2, Some(h3)) =>
      if (h1.children.length <= h2.children.length) {
        if (h1.children.length == h3.children.length) {
          h2 :: mergeTrees(t1, t2, Some(h1.merge(h3)))
        } else {
          h3 :: mergeTrees(l1, l2, None)
        }
      } else {
        if (h2.children.length == h3.children.length) {
          mergeTrees(l1, t2, Some(h2.merge(h3)))
        } else {
          h3 :: mergeTrees(l1, l2, None)
        }
      }
  }

  def apply[T: Ordering](xs: T*): BinomialHeap[T] = (new BinomialHeap(Nil) /: xs) (_.enqueue(_))
}
