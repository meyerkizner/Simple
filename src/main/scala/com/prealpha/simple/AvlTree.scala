package com.prealpha.simple

sealed abstract class AvlTree[T] extends SimpleSet[T] {
  override def add(elem: T): AvlTree[T]

  override def remove(elem: T): AvlTree[T]

  protected def height: Int

  protected def max: Option[T]
}

object AvlTree {
  private case class Empty[T: Ordering]() extends AvlTree[T] {
    override def add(elem: T): AvlTree[T] = Node(this, elem, this)

    override def remove(elem: T): AvlTree[T] = this

    override def contains(elem: T): Boolean = false

    override def fold[U](combine: (T, U) => U, base: U): U = base

    override protected val height: Int = -1

    override protected val max: Option[T] = None
  }

  private case class Node[T: Ordering](
      private val left: AvlTree[T],
      private val value: T,
      private val right: AvlTree[T])
    extends AvlTree[T] {

    import scala.math.Ordering.Implicits._

    override protected lazy val height = Math.max(left.height, right.height) + 1

    private lazy val balanceFactor = left.height - right.height

    override def add(elem: T): AvlTree[T] = {
      if (elem < value) {
        Node(left.add(elem), value, right).balance
      } else if (elem > value) {
        Node(left, value, right.add(elem)).balance
      } else {
        this
      }
    }

    override def remove(elem: T): AvlTree[T] = {
      if (elem < value) {
        Node(left.remove(elem), value, right).balance
      } else if (elem > value) {
        Node(left, value, right.remove(elem)).balance
      } else {
        // locate the in-order predecessor of this value
        left.max match {
          case None => right
          case Some(pred) => Node(left.remove(pred), pred, right).balance
        }
      }
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

    private def balance: AvlTree[T] = (this, balanceFactor) match {
      case (Node(t0, x, Node(Node(t1, y, t2), z, t3)), -2) =>
        Node(Node(t0, x, t1), y, Node(t2, z, t3))
      case (Node(t0, x, Node(t1, y, Node(t2, z, t3))), -2) =>
        Node(Node(t0, x, t1), y, Node(t2, z, t3))
      case (Node(Node(t0, x, Node(t1, y, t2)), z, t3), 2) =>
        Node(Node(t0, x, t1), y, Node(t2, z, t3))
      case (Node(Node(Node(t0, x, t1), y, t2), z, t3), 2) =>
        Node(Node(t0, x, t1), y, Node(t2, z, t3))
      case _ =>
        this
    }
  }

  def apply[T: Ordering](xs: T*): AvlTree[T] = xs match {
    case Nil => Empty[T]()
    case hd :: tl => apply(tl: _*).add(hd)
  }
}
