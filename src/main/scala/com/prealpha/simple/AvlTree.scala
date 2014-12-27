package com.prealpha.simple

sealed abstract class AvlTree[T] extends SimpleSet[T] {
  override def add(elem: T): AvlTree[T]

  override def remove(elem: T): AvlTree[T]

  protected def height: Int

  protected def max: Option[T]
}

object AvlTree {
  private case class Empty[T: Ordering]() extends AvlTree[T] {
    override def add(elem: T): AvlTree[T] = Node(this, elem, 0, this)

    override def remove(elem: T): AvlTree[T] = this

    override def contains(elem: T): Boolean = false

    override def fold[U](combine: (T, U) => U, base: U): U = base

    override protected def height: Int = -1

    override protected def max: Option[T] = None
  }

  private case class Node[T: Ordering](private val left: AvlTree[T], private val value: T, protected val height: Int,
                                       private val right: AvlTree[T]) extends AvlTree[T] {
    import scala.math.Ordering.Implicits._

    override def add(elem: T): AvlTree[T] = {
      val inserted =
        if (elem < value) {
          val newLeft = left.add(elem)
          Node(newLeft, value, Math.max(newLeft.height, right.height), right)
        } else if (elem > value) {
          val newRight = right.add(elem)
          Node(left, value, Math.max(left.height, newRight.height), right)
        } else {
          this
        }
      balance(inserted)
    }

    override def remove(elem: T): AvlTree[T] = {
      val deleted =
        if (elem < value) {
          val newLeft = left.remove(elem)
          Node(newLeft, value, Math.max(newLeft.height, right.height), right)
        } else if (elem > value) {
          val newRight = right.add(elem)
          Node(left, value, Math.max(left.height, newRight.height), right)
        } else {
          // locate the in-order predecessor of this value
          left.max match {
            case None => right
            case Some(pred) =>
              val newLeft = left.remove(pred)
              Node(newLeft, pred, Math.max(newLeft.height, right.height), right)
          }
        }
      balance(deleted)
    }

    override def contains(elem: T): Boolean = {
      if (elem < value) {
        left.contains(elem)
      } else if (elem > value) {
        right.contains(elem)
      } else {
        true
      }
    }

    override def fold[U](combine: (T, U) => U, base: U): U = {
      right.fold(combine, combine(value, left.fold(combine, base)))
    }

    override protected def max: Option[T] = right.max match {
      case None => Some(value)
      case Some(max) => Some(max)
    }
  }

  private def balance[T: Ordering](tree: AvlTree[T]): AvlTree[T] = tree match {
    case Empty() => tree
    case Node(l, v, h, r) =>
      val balanceFactor = l.height - r.height
      if (balanceFactor < -1) {
        rotateRight(tree)
      } else if (balanceFactor > 1) {
        rotateLeft(tree)
      } else {
        tree
      }
  }

  private def rotateLeft[T: Ordering](tree: AvlTree[T]): AvlTree[T] = tree match {
    case Node(t0, x, _, Node(Node(t1, y, _, t2), z, _, t3)) =>
      val xh = Math.max(t0.height, t1.height)
      val zh = Math.max(t2.height, t3.height)
      val yh = Math.max(xh, zh)
      Node(Node(t0, x, xh, t1), y, yh, Node(t2, z, zh, t3))
    case Node(t0, x, _, Node(t1, y, _, Node(t2, z, _, t3))) =>
      val xh = Math.max(t0.height, t1.height)
      val zh = Math.max(t2.height, t3.height)
      val yh = Math.max(xh, zh)
      Node(Node(t0, x, xh, t1), y, yh, Node(t2, z, zh, t3))
    case _ => tree
  }

  private def rotateRight[T: Ordering](tree: AvlTree[T]): AvlTree[T] = tree match {
    case Node(Node(t0, x, _, Node(t1, y, _, t2)), z, _, t3) =>
      val xh = Math.max(t0.height, t1.height)
      val zh = Math.max(t2.height, t3.height)
      val yh = Math.max(xh, zh)
      Node(Node(t0, x, xh, t1), y, yh, Node(t2, z, zh, t3))
    case Node(Node(Node(t0, x, _, t1), y, _, t2), z, _, t3) =>
      val xh = Math.max(t0.height, t1.height)
      val zh = Math.max(t2.height, t3.height)
      val yh = Math.max(xh, zh)
      Node(Node(t0, x, xh, t1), y, yh, Node(t2, z, zh, t3))
    case _ => tree
  }

  def apply[T: Ordering](xs: T*): AvlTree[T] = xs match {
    case Nil => Empty[T]()
    case hd :: tl => apply(tl: _*).add(hd)
  }
}
