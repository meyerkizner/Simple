package com.prealpha.simple

sealed abstract class TwoThreeTree[T: Ordering] extends SimpleSet[T] {
  protected type SplitNode = (TwoThreeTree.NonEmpty[T], T, TwoThreeTree.NonEmpty[T])

  protected def doAdd(elem: T): Either[SplitNode, TwoThreeTree.NonEmpty[T]]

  override def add(elem: T): TwoThreeTree.NonEmpty[T] = doAdd(elem) match {
    case Left((leftChild, value, rightChild)) =>
      TwoThreeTree.TwoNode(leftChild, value, rightChild)
    case Right(tree) =>
      tree
  }
}

object TwoThreeTree {
  import scala.math.Ordering.Implicits._

  private case class Empty[T: Ordering]() extends TwoThreeTree[T] {
    override protected def doAdd(elem: T): Either[SplitNode, NonEmpty[T]] = Right(TwoLeaf(elem))

    override def remove(elem: T): SimpleSet[T] = this

    override def contains(elem: T): Boolean = false

    override def fold[U](base: U)(combine: (U, T) => U): U = base
  }

  protected sealed abstract class NonEmpty[T: Ordering] extends TwoThreeTree[T]

  private case class TwoLeaf[T: Ordering](private val value: T) extends NonEmpty[T] {
    override protected def doAdd(elem: T): Either[SplitNode, NonEmpty[T]] = {
      if (elem < value) {
        Right(ThreeLeaf(elem, value))
      } else if (elem > value) {
        Right(ThreeLeaf(value, elem))
      } else {
        Right(this)
      }
    }

    override def contains(elem: T): Boolean = {
      elem == value
    }

    override def fold[U](base: U)(combine: (U, T) => U): U = combine(base, value)
  }

  private case class TwoNode[T: Ordering](
      private val left: NonEmpty[T],
      private val value: T,
      private val right: NonEmpty[T])
    extends NonEmpty[T] {

    override protected def doAdd(elem: T): Either[SplitNode, NonEmpty[T]] = {
      if (elem < value) {
        left.doAdd(elem) match {
          case Left((leftChild, leftValue, middleChild)) =>
            Right(ThreeNode(leftChild, leftValue, middleChild, value, right))
          case Right(leftChild) =>
            Right(TwoNode(leftChild, value, right))
        }
      } else if (elem > value) {
        right.doAdd(elem) match {
          case Left((middleChild, rightValue, rightChild)) =>
            Right(ThreeNode(left, value, middleChild, rightValue, rightChild))
          case Right(rightChild) =>
            Right(TwoNode(left, value, rightChild))
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

  private case class ThreeLeaf[T: Ordering](
      private val leftValue: T,
      private val rightValue: T)
    extends NonEmpty[T] {

    override protected def doAdd(elem: T): Either[SplitNode, NonEmpty[T]] = {
      if (elem < leftValue) {
        val leftChild = TwoLeaf(elem)
        val rightChild = TwoLeaf(rightValue)
        Left((leftChild, leftValue, rightChild))
      } else if (elem > rightValue) {
        val leftChild = TwoLeaf(leftValue)
        val rightChild = TwoLeaf(elem)
        Left((leftChild, rightValue, rightChild))
      } else if (elem != leftValue && elem != rightValue) {
        val leftChild = TwoLeaf(leftValue)
        val rightChild = TwoLeaf(rightValue)
        Left((leftChild, elem, rightChild))
      } else {
        Right(this)
      }
    }

    override def contains(elem: T): Boolean = {
      elem == leftValue || elem == rightValue
    }

    override def fold[U](base: U)(combine: (U, T) => U): U = {
      combine(combine(base, leftValue), rightValue)
    }
  }

  private case class ThreeNode[T: Ordering](
      private val left: NonEmpty[T],
      private val leftValue: T,
      private val middle: NonEmpty[T],
      private val rightValue: T,
      private val right: NonEmpty[T])
    extends NonEmpty[T] {

    override protected def doAdd(elem: T): Either[SplitNode, NonEmpty[T]] = {
      if (elem < leftValue) {
        left.doAdd(elem) match {
          case Left((farLeft, value, leftMiddle)) =>
            val leftChild = TwoNode(farLeft, value, leftMiddle)
            val rightChild = TwoNode(middle, rightValue, right)
            Left((leftChild, leftValue, rightChild))
          case Right(leftChild) =>
            Right(ThreeNode(leftChild, leftValue, middle, rightValue, right))
        }
      } else if (elem > rightValue) {
        right.doAdd(elem) match {
          case Left((rightMiddle, value, farRight)) =>
            val leftChild = TwoNode(left, leftValue, middle)
            val rightChild = TwoNode(rightMiddle, value, farRight)
            Left((leftChild, rightValue, rightChild))
          case Right(rightChild) =>
            Right(ThreeNode(left, leftValue, middle, rightValue, rightChild))
        }
      } else if (elem != leftValue && elem != rightValue) {
        middle.doAdd(elem) match {
          case Left((leftMiddle, value, rightMiddle)) =>
            val leftChild = TwoNode(left, leftValue, leftMiddle)
            val rightChild = TwoNode(rightMiddle, rightValue, right)
            Left((leftChild, value, rightChild))
          case Right(middleChild) =>
            Right(ThreeNode(left, leftValue, middleChild, rightValue, right))
        }
      } else {
        Right(this)
      }
    }

    override def contains(elem: T): Boolean = {
      if (elem < leftValue) {
        left.contains(elem)
      } else if (elem > rightValue) {
        right.contains(elem)
      } else if (elem != leftValue && elem != rightValue) {
        middle.contains(elem)
      } else {
        true
      }
    }

    override def fold[U](base: U)(combine: (U, T) => U): U = {
      right.fold(combine(middle.fold(combine(left.fold(base)(combine), leftValue))(combine), rightValue))(combine)
    }
  }
}
