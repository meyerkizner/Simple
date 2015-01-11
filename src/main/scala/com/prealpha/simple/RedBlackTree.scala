package com.prealpha.simple

sealed abstract class RedBlackTree[T] extends SimpleSet[T] {
  protected def doAdd(elem: T): Either[RedBlackTree.BlackNonEmpty[T], RedBlackTree[T]]

  override final def add(elem: T): RedBlackTree[T] = doAdd(elem) match {
    case Left(tree) => tree
    case Right(tree) => tree
  }
}

object RedBlackTree {
  import scala.math.Ordering.Implicits._

  protected abstract class BlackNode[T] extends RedBlackTree[T] {
    override protected[RedBlackTree] def doAdd(elem: T): Right[BlackNonEmpty[T], RedBlackTree[T]]
  }

  private case class Empty[T: Ordering]() extends BlackNode[T] {
    override protected[RedBlackTree] def doAdd(elem: T): Right[BlackNonEmpty[T], RedBlackTree[T]] = {
      Right(RedNode(this, elem, this))
    }

    override def remove(elem: T): RedBlackTree[T] = this

    override def contains(elem: T): Boolean = false

    override def fold[U](base: U)(combine: (U, T) => U): U = base
  }

  private case class RedNode[T: Ordering](
      private val left: BlackNode[T],
      private val value: T,
      private val right: BlackNode[T])
    extends RedBlackTree[T] {

    override protected def doAdd(elem: T): Either[BlackNonEmpty[T], RedBlackTree[T]] = {
      if (elem < value) {
        left.doAdd(elem) match {
          case Right(RedNode(farLeft, leftValue, middleLeft)) =>
            val leftChild = RedNode(farLeft, leftValue, middleLeft)
            Left(BlackNonEmpty(leftChild, value, right))
          case Right(leftChild) =>
            Right(RedNode(leftChild.asInstanceOf[BlackNode[T]], value, right))
        }
      } else if (elem > value) {
        right.doAdd(elem) match {
          case Right(RedNode(middleRight, rightValue, farRight)) =>
            val rightChild = RedNode(middleRight, rightValue, farRight)
            Left(BlackNonEmpty(left, value, rightChild))
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

    override protected[RedBlackTree] def doAdd(elem: T): Right[BlackNonEmpty[T], RedBlackTree[T]] = {
      if (elem < value) {
        (left.doAdd(elem), right) match {
          case (Left(leftChild), RedNode(rightMiddle, rightValue, farRight)) =>
            // re-color the right child as black and this node as red to restore balance
            val rightChild = BlackNonEmpty(rightMiddle, rightValue, farRight)
            Right(RedNode(leftChild, value, rightChild))
          case (Left(BlackNonEmpty(RedNode(_, _, _), _, RedNode(_, _, _))), _) =>
            throw new IllegalStateException("illegal imbalance indicator")
          case (Left(BlackNonEmpty(farLeft, leftValue, RedNode(leftMiddle, middleValue, rightMiddle))), _) =>
            // double rotation to make both child nodes red
            val leftChild = RedNode(farLeft.asInstanceOf[BlackNode[T]], leftValue, leftMiddle)
            val rightChild = RedNode(rightMiddle, value, right.asInstanceOf[BlackNode[T]])
            Right(BlackNonEmpty(leftChild, middleValue, rightChild))
          case (Left(BlackNonEmpty(RedNode(farLeft, leftValue, leftMiddle), middleValue, rightMiddle)), _) =>
            // single rotation to make both child nodes red
            val leftChild = RedNode(farLeft, leftValue, leftMiddle)
            val rightChild = RedNode(rightMiddle.asInstanceOf[BlackNode[T]], value, right.asInstanceOf[BlackNode[T]])
            Right(BlackNonEmpty(leftChild, middleValue, rightChild))
          case (Left(_), _) =>
            throw new IllegalStateException("illegal imbalance indicator")
          case (Right(leftChild), _) =>
            Right(BlackNonEmpty(leftChild, value, right))
        }
      } else if (elem > value) {
        (left, right.doAdd(elem)) match {
          case (RedNode(farLeft, leftValue, leftMiddle), Left(rightChild)) =>
            // re-color the left child as black and this node as red to restore balance
            val leftChild = BlackNonEmpty(farLeft, leftValue, leftMiddle)
            Right(RedNode(leftChild, value, rightChild))
          case (_, Left(BlackNonEmpty(RedNode(_, _, _), _, RedNode(_, _, _)))) =>
            throw new IllegalStateException("illegal imbalance indicator")
          case (_, Left(BlackNonEmpty(RedNode(leftMiddle, middleValue, rightMiddle), rightValue, farRight))) =>
            // double rotation to make both child nodes red
            val leftChild = RedNode(left.asInstanceOf[BlackNode[T]], value, leftMiddle)
            val rightChild = RedNode(rightMiddle, rightValue, farRight.asInstanceOf[BlackNode[T]])
            Right(BlackNonEmpty(leftChild, middleValue, rightChild))
          case (_, Left(BlackNonEmpty(leftMiddle, middleValue, RedNode(rightMiddle, rightValue, farRight)))) =>
            // single rotation to make both child nodes red
            val leftChild = RedNode(left.asInstanceOf[BlackNode[T]], value, leftMiddle.asInstanceOf[BlackNode[T]])
            val rightChild = RedNode(rightMiddle, rightValue, farRight)
            Right(BlackNonEmpty(leftChild, middleValue, rightChild))
          case (_, Left(_)) =>
            throw new IllegalStateException("illegal imbalance indicator")
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
