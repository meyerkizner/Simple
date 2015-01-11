package com.prealpha.simple

sealed abstract class RedBlackTree[T: Ordering] extends SimpleSet[T] {
  protected type InvalidRed = Either[(RedBlackTree.RedNode[T], T, RedBlackTree.BlackNode[T]),
    (RedBlackTree.BlackNode[T], T, RedBlackTree.RedNode[T])]

  protected final def repaintInvalid(invalid: InvalidRed): RedBlackTree.BlackNode[T] = {
    val (left, value, right) = invalid.merge
    RedBlackTree.BlackNonEmpty(left, value, right)
  }

  protected def doAdd(elem: T): Either[InvalidRed, RedBlackTree[T]]

  override final def add(elem: T): RedBlackTree[T] = doAdd(elem) match {
    case Left(invalid) => repaintInvalid(invalid)
    case Right(tree) => tree
  }
}

object RedBlackTree {
  import scala.math.Ordering.Implicits._

  protected abstract sealed class BlackNode[T: Ordering] extends RedBlackTree[T] {
    override protected[RedBlackTree] def doAdd(elem: T): Right[InvalidRed, RedBlackTree[T]]
  }

  private case class Empty[T: Ordering]() extends BlackNode[T] {
    override protected[RedBlackTree] def doAdd(elem: T): Right[InvalidRed, RedBlackTree[T]] = {
      Right(RedNode(this, elem, this))
    }

    override def remove(elem: T): RedBlackTree[T] = this

    override def contains(elem: T): Boolean = false

    override def fold[U](base: U)(combine: (U, T) => U): U = base
  }

  protected case class RedNode[T: Ordering](
      private val left: BlackNode[T],
      private val value: T,
      private val right: BlackNode[T])
    extends RedBlackTree[T] {

    override protected def doAdd(elem: T): Either[InvalidRed, RedBlackTree[T]] = {
      if (elem < value) {
        left.doAdd(elem) match {
          case Right(leftChild: RedNode[T]) =>
            Left(Left((leftChild, value, right)))
          case Right(leftChild: BlackNode[T]) =>
            Right(RedNode(leftChild, value, right))
        }
      } else if (elem > value) {
        right.doAdd(elem) match {
          case Right(rightChild: RedNode[T]) =>
            Left(Right((left, value, rightChild)))
          case Right(rightChild: BlackNode[T]) =>
            Right(RedNode(left, value, rightChild))
        }
      } else {
        Right(this)
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

    override def fold[U](base: U)(combine: (U, T) => U): U = {
      right.fold(combine(left.fold(base)(combine), value))(combine)
    }
  }

  protected case class BlackNonEmpty[T: Ordering](
      private val left: RedBlackTree[T],
      private val value: T,
      private val right: RedBlackTree[T])
    extends BlackNode[T] {

    override protected[RedBlackTree] def doAdd(elem: T): Right[InvalidRed, RedBlackTree[T]] = {
      if (elem < value) {
        (left.doAdd(elem), right) match {
          case (Left(invalid), RedNode(rightMiddle, rightValue, farRight)) =>
            // re-color both children as black and this node as red to restore balance
            val leftChild = repaintInvalid(invalid)
            val rightChild = BlackNonEmpty(rightMiddle, rightValue, farRight)
            Right(RedNode(leftChild, value, rightChild))
          case (Left(Left((RedNode(farLeft, leftValue, leftMiddle), middleValue, rightMiddle))),
                farRight: BlackNode[T]) =>
            // single rotation to make both child nodes red
            val leftChild = RedNode(farLeft, leftValue, leftMiddle)
            val rightChild = RedNode(rightMiddle, value, farRight)
            Right(BlackNonEmpty(leftChild, middleValue, rightChild))
          case (Left(Right((farLeft, leftValue, RedNode(leftMiddle, middleValue, rightMiddle)))),
                farRight: BlackNode[T]) =>
            // double rotation to make both child nodes red
            val leftChild = RedNode(farLeft, leftValue, leftMiddle)
            val rightChild = RedNode(rightMiddle, value, farRight)
            Right(BlackNonEmpty(leftChild, middleValue, rightChild))
          case (Right(leftChild), rightChild) =>
            Right(BlackNonEmpty(leftChild, value, rightChild))
        }
      } else if (elem > value) {
        (left, right.doAdd(elem)) match {
          case (RedNode(farLeft, leftValue, leftMiddle), Left(invalid)) =>
            // re-color both children as black and this node as red to restore balance
            val leftChild = BlackNonEmpty(farLeft, leftValue, leftMiddle)
            val rightChild = repaintInvalid(invalid)
            Right(RedNode(leftChild, value, rightChild))
          case (farLeft: BlackNode[T],
                Left(Left((RedNode(leftMiddle, middleValue, rightMiddle), rightValue, farRight)))) =>
            // double rotation to make both child nodes red
            val leftChild = RedNode(farLeft, value, leftMiddle)
            val rightChild = RedNode(rightMiddle, rightValue, farRight)
            Right(BlackNonEmpty(leftChild, middleValue, rightChild))
          case (farLeft: BlackNode[T],
                Left(Right((leftMiddle, middleValue, RedNode(rightMiddle, rightValue, farRight))))) =>
            // single rotation to make both child nodes red
            val leftChild = RedNode(farLeft, value, leftMiddle)
            val rightChild = RedNode(rightMiddle, rightValue, farRight)
            Right(BlackNonEmpty(leftChild, middleValue, rightChild))
          case (leftChild, Right(rightChild)) =>
            Right(BlackNonEmpty(leftChild, value, rightChild))
        }
      } else {
        Right(this)
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

    override def fold[U](base: U)(combine: (U, T) => U): U = {
      right.fold(combine(left.fold(base)(combine), value))(combine)
    }
  }
}
