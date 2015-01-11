package com.prealpha.simple

sealed abstract class RedBlackTree[T: Ordering] extends SimpleSet[T] {
  protected type InvalidRed = Either[(RedBlackTree.RedNode[T], T, RedBlackTree.BlackNode[T]),
    (RedBlackTree.BlackNode[T], T, RedBlackTree.RedNode[T])]

  protected final def repaintInvalid(invalid: InvalidRed) = {
    val (left, value, right) = invalid.merge
    RedBlackTree.BlackNonEmpty(left, value, right)
  }

  protected def doAdd(elem: T): Either[InvalidRed, RedBlackTree[T]]

  override final def add(elem: T): RedBlackTree[T] = doAdd(elem) match {
    case Left(Left((left, value, right))) =>
      RedBlackTree.BlackNonEmpty(left, value, right)
    case Left(Right((left, value, right))) =>
      RedBlackTree.BlackNonEmpty(left, value, right)
    case Right(tree) => tree
  }
}

object RedBlackTree {
  import scala.math.Ordering.Implicits._

  protected abstract class BlackNode[T: Ordering] extends RedBlackTree[T] {
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
          case Right(RedNode(farLeft, leftValue, middleLeft)) =>
            val leftChild = RedNode(farLeft, leftValue, middleLeft)
            Left(Left((leftChild, value, right)))
          case Right(leftChild) =>
            Right(RedNode(leftChild.asInstanceOf[BlackNode[T]], value, right))
        }
      } else if (elem > value) {
        right.doAdd(elem) match {
          case Right(RedNode(middleRight, rightValue, farRight)) =>
            val rightChild = RedNode(middleRight, rightValue, farRight)
            Left(Right((left, value, rightChild)))
          case Right(rightChild) =>
            Right(RedNode(left, value, rightChild.asInstanceOf[BlackNode[T]]))
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
          case (Left(Left((RedNode(farLeft, leftValue, leftMiddle), middleValue, rightMiddle))), _) =>
            // single rotation to make both child nodes red
            val leftChild = RedNode(farLeft, leftValue, leftMiddle)
            val rightChild = RedNode(rightMiddle, value, right.asInstanceOf[BlackNode[T]])
            Right(BlackNonEmpty(leftChild, middleValue, rightChild))
          case (Left(Right((farLeft, leftValue, RedNode(leftMiddle, middleValue, rightMiddle)))), _) =>
            // double rotation to make both child nodes red
            val leftChild = RedNode(farLeft, leftValue, leftMiddle)
            val rightChild = RedNode(rightMiddle, value, right.asInstanceOf[BlackNode[T]])
            Right(BlackNonEmpty(leftChild, middleValue, rightChild))
          case (Right(leftChild), _) =>
            Right(BlackNonEmpty(leftChild, value, right))
        }
      } else if (elem > value) {
        (left, right.doAdd(elem)) match {
          case (RedNode(farLeft, leftValue, leftMiddle), Left(invalid)) =>
            // re-color both children as black and this node as red to restore balance
            val leftChild = BlackNonEmpty(farLeft, leftValue, leftMiddle)
            val rightChild = repaintInvalid(invalid)
            Right(RedNode(leftChild, value, rightChild))
          case (_, Left(Left((RedNode(leftMiddle, middleValue, rightMiddle), rightValue, farRight)))) =>
            // double rotation to make both child nodes red
            val leftChild = RedNode(left.asInstanceOf[BlackNode[T]], value, leftMiddle)
            val rightChild = RedNode(rightMiddle, rightValue, farRight)
            Right(BlackNonEmpty(leftChild, middleValue, rightChild))
          case (_, Left(Right((leftMiddle, middleValue, RedNode(rightMiddle, rightValue, farRight))))) =>
            // single rotation to make both child nodes red
            val leftChild = RedNode(left.asInstanceOf[BlackNode[T]], value, leftMiddle)
            val rightChild = RedNode(rightMiddle, rightValue, farRight)
            Right(BlackNonEmpty(leftChild, middleValue, rightChild))
          case (_, Right(rightChild)) =>
            Right(BlackNonEmpty(left, value, rightChild))
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
