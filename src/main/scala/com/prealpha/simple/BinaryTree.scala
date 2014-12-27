package com.prealpha.simple

sealed abstract class BinaryTree[T] extends SimpleSet[T] {
  override def add(elem: T): BinaryTree[T]

  override def remove(elem: T): BinaryTree[T]

  protected[simple] def max: Option[T]
}

private case class Empty[T: Ordering]() extends BinaryTree[T] {
  override def add(elem: T): BinaryTree[T] = Node(this, elem, this)

  override def remove(elem: T): BinaryTree[T] = this

  override def contains(elem: T): Boolean = false

  override def fold[U](combine: (T, U) => U, base: U): U = base

  override protected[simple] def max: Option[T] = None
}

private case class Node[T: Ordering](private val left: BinaryTree[T], private val value: T,
                                     private val right: BinaryTree[T]) extends BinaryTree[T] {
  import scala.math.Ordering.Implicits._

  override def add(elem: T): BinaryTree[T] = {
    if (elem < value) {
      Node(left.add(elem), value, right)
    } else if (elem > value) {
      Node(left, value, right.add(elem))
    } else {
      this
    }
  }

  override def remove(elem: T): BinaryTree[T] = {
    if (elem < value) {
      Node(left.remove(elem), value, right)
    } else if (elem > value) {
      Node(left, value, right.remove(elem))
    } else {
      // match on the in-order predecessor of this value
      left.max match {
        case None => right
        case Some(pred) => Node(left.remove(pred), pred, right)
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

  override protected[simple] def max: Option[T] = right.max match {
    case None => Some(value)
    case Some(max) => Some(max)
  }
}

object BinaryTree {
  def apply[T](xs: T*): BinaryTree[T] = xs match {
    case Nil => Empty[T]()
    case hd :: tl => apply[T](tl: _*).add(hd)
  }
}
