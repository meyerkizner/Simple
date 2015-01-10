package com.prealpha.simple

sealed abstract class TwoThreeTree[T: Ordering] extends SimpleSet[T] {
  protected type SplitNode = (TwoThreeTree.NonEmpty[T], T, TwoThreeTree.NonEmpty[T])

  protected def doAdd(elem: T): Either[SplitNode, TwoThreeTree.NonEmpty[T]]

  override final def add(elem: T): TwoThreeTree.NonEmpty[T] = doAdd(elem) match {
    case Left((leftChild, value, rightChild)) =>
      TwoThreeTree.TwoNode(leftChild, value, rightChild)
    case Right(tree) =>
      tree
  }

  protected def doRemove(elem: T): Option[Either[TwoThreeTree.NonEmpty[T], TwoThreeTree.NonEmpty[T]]]

  override final def remove(elem: T): TwoThreeTree[T] = doRemove(elem) match {
    case None => TwoThreeTree.Empty()
    case Some(Left(tree)) => tree
    case Some(Right(tree)) => tree
  }
}

object TwoThreeTree {
  import scala.math.Ordering.Implicits._

  private case class Empty[T: Ordering]() extends TwoThreeTree[T] {
    override protected def doAdd(elem: T): Either[SplitNode, NonEmpty[T]] = Right(TwoLeaf(elem))

    override protected def doRemove(elem: T): Option[Either[NonEmpty[T], NonEmpty[T]]] = None

    override def contains(elem: T): Boolean = false

    override def fold[U](base: U)(combine: (U, T) => U): U = base
  }

  protected sealed abstract class NonEmpty[T: Ordering] extends TwoThreeTree[T] {
    protected[TwoThreeTree] def max: T
  }

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

    override protected def doRemove(elem: T): Option[Either[NonEmpty[T], NonEmpty[T]]] = {
      if (elem == value) {
        None
      } else {
        Some(Right(this))
      }
    }

    override def contains(elem: T): Boolean = {
      elem == value
    }

    override def fold[U](base: U)(combine: (U, T) => U): U = combine(base, value)

    override protected[TwoThreeTree] def max: T = value
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

    override protected def doRemove(elem: T): Option[Either[NonEmpty[T], NonEmpty[T]]] = {
      if (elem <= value) {
        val anchor = if (elem == value) left.max else value
        val toRemove = if (elem == value) anchor else elem
        left.doRemove(toRemove) match {
          case None =>
            right match {
              case TwoLeaf(rightValue) =>
                Some(Left(ThreeLeaf(anchor, rightValue)))
              case ThreeLeaf(middleValue, rightValue) =>
                val leftChild = TwoLeaf(anchor)
                val rightChild = TwoLeaf(rightValue)
                Some(Right(TwoNode(leftChild, middleValue, rightChild)))
              case TwoNode(_, _, _) | ThreeNode(_, _, _, _, _) =>
                throw new IllegalStateException("unbalanced TwoThreeTree")
            }
          case Some(Left(orphan)) =>
            right match {
              case TwoNode(middleChild, rightValue, rightChild) =>
                Some(Left(ThreeNode(orphan, anchor, middleChild, rightValue, rightChild)))
              case ThreeNode(leftMiddleChild, middleValue, rightMiddleChild, rightValue, farRightChild) =>
                val leftChild = TwoNode(orphan, anchor, leftMiddleChild)
                val rightChild = TwoNode(rightMiddleChild, rightValue, farRightChild)
                Some(Right(TwoNode(leftChild, middleValue, rightChild)))
              case TwoLeaf(_) | ThreeLeaf(_, _) =>
                throw new IllegalStateException("unbalanced TwoThreeTree")
            }
          case Some(Right(leftChild)) =>
            Some(Right(TwoNode(leftChild, anchor, right)))
        }
      } else {
        right.doRemove(elem) match {
          case None =>
            left match {
              case TwoLeaf(leftValue) =>
                Some(Left(ThreeLeaf(leftValue, value)))
              case ThreeLeaf(leftValue, middleValue) =>
                val leftChild = TwoLeaf(leftValue)
                val rightChild = TwoLeaf(value)
                Some(Right(TwoNode(leftChild, middleValue, rightChild)))
              case TwoNode(_, _, _) | ThreeNode(_, _, _, _, _) =>
                throw new IllegalStateException("unbalanced TwoThreeTree")
            }
          case Some(Left(orphan)) =>
            left match {
              case TwoNode(leftChild, leftValue, middleChild) =>
                Some(Left(ThreeNode(leftChild, leftValue, middleChild, value, orphan)))
              case ThreeNode(farLeftChild, leftValue, leftMiddleChild, middleValue, rightMiddleChild) =>
                val leftChild = TwoNode(farLeftChild, leftValue, leftMiddleChild)
                val rightChild = TwoNode(rightMiddleChild, value, orphan)
                Some(Right(TwoNode(leftChild, middleValue, rightChild)))
              case TwoLeaf(_) | ThreeLeaf(_, _) =>
                throw new IllegalStateException("unbalanced TwoThreeTree")
            }
          case Some(Right(rightChild)) =>
            Some(Right(TwoNode(left, value, rightChild)))
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

    override def fold[U](base: U)(combine: (U, T) => U): U = {
      right.fold(combine(left.fold(base)(combine), value))(combine)
    }

    override protected[TwoThreeTree] def max: T = right.max
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

    override protected def doRemove(elem: T): Option[Either[NonEmpty[T], NonEmpty[T]]] = {
      if (elem == leftValue) {
        Some(Right(TwoLeaf(rightValue)))
      } else if (elem == rightValue) {
        Some(Right(TwoLeaf(leftValue)))
      } else {
        Some(Right(this))
      }
    }

    override def contains(elem: T): Boolean = {
      elem == leftValue || elem == rightValue
    }

    override def fold[U](base: U)(combine: (U, T) => U): U = {
      combine(combine(base, leftValue), rightValue)
    }

    override protected[TwoThreeTree] def max: T = rightValue
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

    override protected def doRemove(elem: T): Option[Either[NonEmpty[T], NonEmpty[T]]] = {
      if (elem <= leftValue) {
        val anchor = if (elem == leftValue) left.max else leftValue
        val toRemove = if (elem == leftValue) anchor else elem
        left.doRemove(toRemove) match {
          case None =>
            middle match {
              case TwoLeaf(leftMiddleValue) =>
                val leftChild = ThreeLeaf(anchor, leftMiddleValue)
                Some(Right(TwoNode(leftChild, rightValue, right)))
              case ThreeLeaf(leftMiddleValue, middleValue) =>
                val leftChild = TwoLeaf(anchor)
                val middleChild = TwoLeaf(middleValue)
                Some(Right(ThreeNode(leftChild, leftMiddleValue, middleChild, rightValue, right)))
              case TwoNode(_, _, _) | ThreeNode(_, _, _, _, _) =>
                throw new IllegalStateException("unbalanced TwoThreeTree")
            }
          case Some(Left(orphan)) =>
            middle match {
              case TwoNode(leftMiddleChild, middleValue, rightMiddleChild) =>
                val leftChild = TwoNode(orphan, anchor, leftMiddleChild)
                val rightChild = TwoNode(rightMiddleChild, rightValue, right)
                Some(Right(TwoNode(leftChild, middleValue, rightChild)))
              case ThreeNode(middleLeftChild, leftMiddleValue, leftMiddleChild, middleValue, rightMiddleChild) =>
                val leftChild = TwoNode(orphan, anchor, middleLeftChild)
                val middleChild = TwoNode(leftMiddleChild, middleValue, rightMiddleChild)
                Some(Right(ThreeNode(leftChild, leftMiddleValue, middleChild, rightValue, right)))
              case TwoLeaf(_) | ThreeLeaf(_, _) =>
                throw new IllegalStateException("unbalanced TwoThreeTree")
            }
          case Some(Right(leftChild)) =>
            Some(Right(ThreeNode(leftChild, anchor, middle, rightValue, right)))
        }
      } else if (elem <= rightValue) {
        val anchor = if (elem == rightValue) middle.max else rightValue
        val toRemove = if (elem == rightValue) anchor else elem
        middle.doRemove(toRemove) match {
          case None =>
            left match {
              case TwoLeaf(farLeftValue) =>
                right match {
                  case TwoLeaf(_) =>
                    val leftChild = ThreeLeaf(farLeftValue, leftValue)
                    Some(Right(TwoNode(leftChild, anchor, right)))
                  case ThreeLeaf(middleValue, farRightValue) =>
                    val middleChild = TwoLeaf(anchor)
                    val rightChild = TwoLeaf(farRightValue)
                    Some(Right(ThreeNode(left, leftValue, middleChild, middleValue, rightChild)))
                  case TwoNode(_, _, _) | ThreeNode(_, _, _, _, _) =>
                    throw new IllegalStateException("unbalanced TwoThreeTree")
                }
              case ThreeLeaf(farLeftValue, middleValue) =>
                val leftChild = TwoLeaf(farLeftValue)
                val middleChild = TwoLeaf(middleValue)
                Some(Right(ThreeNode(leftChild, leftValue, middleChild, anchor, right)))
              case TwoNode(_, _, _) | ThreeNode(_, _, _, _, _) =>
                throw new IllegalStateException("unbalanced TwoThreeTree")
            }
          case Some(Left(orphan)) =>
            left match {
              case TwoNode(farLeftChild, farLeftValue, middleLeftChild) =>
                right match {
                  case TwoNode(_, _, _) =>
                    val leftChild = ThreeNode(farLeftChild, farLeftValue, middleLeftChild, leftValue, orphan)
                    Some(Right(TwoNode(leftChild, anchor, right)))
                  case ThreeNode(leftMiddleChild, middleValue, rightMiddleChild, farRightValue, farRightChild) =>
                    val middleChild = TwoNode(orphan, anchor, leftMiddleChild)
                    val rightChild = TwoNode(rightMiddleChild, farRightValue, farRightChild)
                    Some(Right(ThreeNode(left, leftValue, middleChild, middleValue, rightChild)))
                  case TwoLeaf(_) | ThreeLeaf(_, _) =>
                    throw new IllegalStateException("unbalanced TwoThreeTree")
                }
              case ThreeNode(farLeftChild, farLeftValue, leftMiddleChild, middleValue, rightMiddleChild) =>
                val leftChild = TwoNode(farLeftChild, farLeftValue, leftMiddleChild)
                val middleChild = TwoNode(rightMiddleChild, leftValue, orphan)
                Some(Right(ThreeNode(leftChild, middleValue, middleChild, anchor, right)))
              case TwoLeaf(_) | ThreeLeaf(_, _) =>
                throw new IllegalStateException("unbalanced TwoThreeTree")
            }
          case Some(Right(middleChild)) =>
            Some(Right(ThreeNode(left, leftValue, middleChild, anchor, right)))
        }
      } else {
        right.doRemove(elem) match {
          case None =>
            middle match {
              case TwoLeaf(middleValue) =>
                val rightChild = ThreeLeaf(middleValue, rightValue)
                Some(Right(TwoNode(left, leftValue, rightChild)))
              case ThreeLeaf(middleValue, rightMiddleValue) =>
                val middleChild = TwoLeaf(middleValue)
                val rightChild = TwoLeaf(rightValue)
                Some(Right(ThreeNode(left, leftValue, middleChild, rightMiddleValue, rightChild)))
              case TwoNode(_, _, _) | ThreeNode(_, _, _, _, _) =>
                throw new IllegalStateException("unbalanced TwoThreeTree")
            }
          case Some(Left(orphan)) =>
            middle match {
              case TwoNode(leftMiddleChild, middleValue, rightMiddleChild) =>
                val rightChild = ThreeNode(leftMiddleChild, middleValue, rightMiddleChild, rightValue, orphan)
                Some(Right(TwoNode(left, leftValue, rightChild)))
              case ThreeNode(leftMiddleChild, middleValue, rightMiddleChild, rightMiddleValue, middleRightChild) =>
                val middleChild = TwoNode(leftMiddleChild, middleValue, rightMiddleChild)
                val rightChild = TwoNode(middleRightChild, rightValue, orphan)
                Some(Right(ThreeNode(left, leftValue, middleChild, rightMiddleValue, rightChild)))
              case TwoLeaf(_) | ThreeLeaf(_, _) =>
                throw new IllegalStateException("unbalanced TwoThreeTree")
            }
          case Some(Right(rightChild)) =>
            Some(Right(ThreeNode(left, leftValue, middle, rightValue, right)))
        }
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

    override protected[TwoThreeTree] def max: T = right.max
  }
}
