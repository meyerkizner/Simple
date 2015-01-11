package com.prealpha.simple

sealed abstract class TwoThreeTree[T: Ordering] extends SimpleSet[T] {
  protected type SplitNode = (TwoThreeTree[T], T, TwoThreeTree[T])

  protected def doAdd(elem: T): Either[SplitNode, TwoThreeTree[T]]

  override final def add(elem: T): TwoThreeTree[T] = doAdd(elem) match {
    case Left((leftChild, value, rightChild)) =>
      TwoThreeTree.TwoNode(leftChild, value, rightChild)
    case Right(tree) =>
      tree
  }

  protected def doRemove(elem: T): Either[TwoThreeTree[T], TwoThreeTree[T]]

  override final def remove(elem: T): TwoThreeTree[T] = doRemove(elem) match {
    case Left(tree) => tree
    case Right(tree) => tree
  }

  protected def max: Option[T]
}

object TwoThreeTree {
  import scala.math.Ordering.Implicits._

  private case class Empty[T: Ordering]() extends TwoThreeTree[T] {
    override protected def doAdd(elem: T): Either[SplitNode, TwoThreeTree[T]] = Right(TwoNode(this, elem, this))

    override protected def doRemove(elem: T): Either[TwoThreeTree[T], TwoThreeTree[T]] = Right(this)

    override def contains(elem: T): Boolean = false

    override def fold[U](base: U)(combine: (U, T) => U): U = base

    override protected def max: Option[T] = None
  }

  private case class TwoNode[T: Ordering](
      private val left: TwoThreeTree[T],
      private val value: T,
      private val right: TwoThreeTree[T])
    extends TwoThreeTree[T] {

    override protected def doAdd(elem: T): Either[SplitNode, TwoThreeTree[T]] = {
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

    override protected def doRemove(elem: T): Either[TwoThreeTree[T], TwoThreeTree[T]] = {
      if (elem <= value) {
        val anchor = if (elem == value) left.max else Some(value)
        anchor map { anchorValue =>
          val toRemove = if (elem == value) anchorValue else elem
          left.doRemove(toRemove) match {
            case Left(orphan) =>
              right match {
                case Empty() =>
                  throw new IllegalStateException("unbalanced TwoThreeTree")
                case TwoNode(middleChild, rightValue, rightChild) =>
                  Left(ThreeNode(orphan, anchorValue, middleChild, rightValue, rightChild))
                case ThreeNode(leftMiddleChild, middleValue, rightMiddleChild, rightValue, farRightChild) =>
                  val leftChild = TwoNode(orphan, anchorValue, leftMiddleChild)
                  val rightChild = TwoNode(rightMiddleChild, rightValue, farRightChild)
                  Right(TwoNode(leftChild, middleValue, rightChild))
              }
            case Right(leftChild) =>
              Right(TwoNode(leftChild, anchorValue, right))
          }
        } getOrElse {
          Left(right)
        }
      } else {
        right.doRemove(elem) match {
          case Left(orphan) =>
            left match {
              case Empty() =>
                throw new IllegalStateException("unbalanced TwoThreeTree")
              case TwoNode(leftChild, leftValue, middleChild) =>
                Left(ThreeNode(leftChild, leftValue, middleChild, value, orphan))
              case ThreeNode(farLeftChild, leftValue, leftMiddleChild, middleValue, rightMiddleChild) =>
                val leftChild = TwoNode(farLeftChild, leftValue, leftMiddleChild)
                val rightChild = TwoNode(rightMiddleChild, value, orphan)
                Right(TwoNode(leftChild, middleValue, rightChild))
            }
          case Right(rightChild) =>
            Right(TwoNode(left, value, rightChild))
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

    override protected def max: Option[T] = right.max match {
      case None => Some(value)
      case Some(max) => Some(max)
    }
  }

  private case class ThreeNode[T: Ordering](
      private val left: TwoThreeTree[T],
      private val leftValue: T,
      private val middle: TwoThreeTree[T],
      private val rightValue: T,
      private val right: TwoThreeTree[T])
    extends TwoThreeTree[T] {

    override protected def doAdd(elem: T): Either[SplitNode, TwoThreeTree[T]] = {
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

    override protected def doRemove(elem: T): Either[TwoThreeTree[T], TwoThreeTree[T]] = {
      if (elem <= leftValue) {
        val anchor = if (elem == leftValue) left.max else Some(leftValue)
        anchor map { anchorValue =>
          val toRemove = if (elem == leftValue) anchorValue else elem
          left.doRemove(toRemove) match {
            case Left(orphan) =>
              middle match {
                case Empty() =>
                  throw new IllegalStateException("unbalanced TwoThreeTree")
                case TwoNode(leftMiddleChild, middleValue, rightMiddleChild) =>
                  val leftChild = TwoNode(orphan, anchorValue, leftMiddleChild)
                  val rightChild = TwoNode(rightMiddleChild, rightValue, right)
                  Right(TwoNode(leftChild, middleValue, rightChild))
                case ThreeNode(middleLeftChild, leftMiddleValue, leftMiddleChild, middleValue, rightMiddleChild) =>
                  val leftChild = TwoNode(orphan, anchorValue, middleLeftChild)
                  val middleChild = TwoNode(leftMiddleChild, middleValue, rightMiddleChild)
                  Right(ThreeNode(leftChild, leftMiddleValue, middleChild, rightValue, right))
              }
            case Right(leftChild) =>
              Right(ThreeNode(leftChild, anchorValue, middle, rightValue, right))
          }
        } getOrElse {
          Right(TwoNode(middle, rightValue, right))
        }
      } else if (elem <= rightValue) {
        val anchor = if (elem == rightValue) middle.max else Some(rightValue)
        anchor map { anchorValue =>
          val toRemove = if (elem == rightValue) anchorValue else elem
          middle.doRemove(toRemove) match {
            case Left(orphan) =>
              left match {
                case Empty() =>
                  throw new IllegalStateException("unbalanced TwoThreeTree")
                case TwoNode(farLeftChild, farLeftValue, middleLeftChild) =>
                  right match {
                    case Empty() =>
                      throw new IllegalStateException("unbalanced TwoThreeTree")
                    case TwoNode(_, _, _) =>
                      val leftChild = ThreeNode(farLeftChild, farLeftValue, middleLeftChild, leftValue, orphan)
                      Right(TwoNode(leftChild, anchorValue, right))
                    case ThreeNode(leftMiddleChild, middleValue, rightMiddleChild, farRightValue, farRightChild) =>
                      val middleChild = TwoNode(orphan, anchorValue, leftMiddleChild)
                      val rightChild = TwoNode(rightMiddleChild, farRightValue, farRightChild)
                      Right(ThreeNode(left, leftValue, middleChild, middleValue, rightChild))
                  }
                case ThreeNode(farLeftChild, farLeftValue, leftMiddleChild, middleValue, rightMiddleChild) =>
                  val leftChild = TwoNode(farLeftChild, farLeftValue, leftMiddleChild)
                  val middleChild = TwoNode(rightMiddleChild, leftValue, orphan)
                  Right(ThreeNode(leftChild, middleValue, middleChild, anchorValue, right))
              }
            case Right(middleChild) =>
              Right(ThreeNode(left, leftValue, middleChild, anchorValue, right))
          }
        } getOrElse {
          Right(TwoNode(left, leftValue, right))
        }
      } else {
        right.doRemove(elem) match {
          case Left(orphan) =>
            middle match {
              case Empty() =>
                throw new IllegalStateException("unbalanced TwoThreeTree")
              case TwoNode(leftMiddleChild, middleValue, rightMiddleChild) =>
                val rightChild = ThreeNode(leftMiddleChild, middleValue, rightMiddleChild, rightValue, orphan)
                Right(TwoNode(left, leftValue, rightChild))
              case ThreeNode(leftMiddleChild, middleValue, rightMiddleChild, rightMiddleValue, middleRightChild) =>
                val middleChild = TwoNode(leftMiddleChild, middleValue, rightMiddleChild)
                val rightChild = TwoNode(middleRightChild, rightValue, orphan)
                Right(ThreeNode(left, leftValue, middleChild, rightMiddleValue, rightChild))
            }
          case Right(rightChild) =>
            Right(ThreeNode(left, leftValue, middle, rightValue, rightChild))
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

    override protected def max: Option[T] = right.max match {
      case None => Some(rightValue)
      case Some(max) => Some(max)
    }
  }
}
