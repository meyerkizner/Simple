package com.prealpha.simple

sealed abstract class TwoThreeTree[T: Ordering] extends SimpleSet[T] {
  protected type SplitNode = (TwoThreeTree.NonEmpty[T], T, TwoThreeTree.NonEmpty[T])

  protected def doAdd(elem: T): Either[SplitNode, TwoThreeTree.NonEmpty[T]]

  override def add(elem: T): TwoThreeTree.NonEmpty[T] = doAdd(elem) match {
    case Left((leftChild, value, rightChild)) =>
      TwoThreeTree.TwoNode(value, Some((leftChild, rightChild)))
    case Right(tree) =>
      tree
  }
}

object TwoThreeTree {
  import scala.math.Ordering.Implicits._

  private case class Empty[T: Ordering]() extends TwoThreeTree[T] {
    override protected def doAdd(elem: T): Either[SplitNode, NonEmpty[T]] = Right(TwoNode(elem, None))

    override def remove(elem: T): SimpleSet[T] = this

    override def contains(elem: T): Boolean = false

    override def fold[U](base: U)(combine: (U, T) => U): U = base
  }

  protected sealed abstract class NonEmpty[T: Ordering] extends TwoThreeTree[T]

  private case class TwoNode[T: Ordering](
      private val value: T,
      private val children: Option[(NonEmpty[T], NonEmpty[T])])
    extends NonEmpty[T] {

    override protected def doAdd(elem: T): Either[SplitNode, NonEmpty[T]] = {
      if (elem < value) {
        children map {
          case (left, right) => left.doAdd(elem) match {
            case Left((leftChild, leftValue, middleChild)) =>
              Right(ThreeNode(leftValue, value, Some((leftChild, middleChild, right))))
            case Right(leftChild) =>
              Right(TwoNode(value, Some((leftChild, right))))
          }
        } getOrElse Right(ThreeNode(elem, value, None))
      } else if (elem > value) {
        children map {
          case (left, right) => right.doAdd(elem) match {
            case Left((middleChild, rightValue, rightChild)) =>
              Right(ThreeNode(value, rightValue, Some((left, middleChild, rightChild))))
            case Right(rightChild) =>
              Right(TwoNode(value, Some((left, rightChild))))
          }
        } getOrElse Right(ThreeNode(value, elem, None))
      } else {
        Right(this)
      }
    }

    override def contains(elem: T): Boolean = {
      if (elem < value) {
        children exists { _._1.contains(elem) }
      } else if (elem > value) {
        children exists { _._2.contains(elem) }
      } else {
        true
      }
    }

    override def fold[U](base: U)(combine: (U, T) => U): U = {
      children map {
        case (left, right) =>
          right.fold(combine(left.fold(base)(combine), value))(combine)
      } getOrElse combine(base, value)
    }
  }

  private case class ThreeNode[T: Ordering](
      private val leftValue: T,
      private val rightValue: T,
      private val children: Option[(NonEmpty[T], NonEmpty[T], NonEmpty[T])])
    extends NonEmpty[T] {

    override protected def doAdd(elem: T): Either[SplitNode, NonEmpty[T]] = {
      if (elem < leftValue) {
        children map {
          case (left, middle, right) => left.doAdd(elem) match {
            case Left((farLeft, value, leftMiddle)) =>
              val leftChild = TwoNode(value, Some((farLeft, leftMiddle)))
              val rightChild = TwoNode(rightValue, Some((middle, right)))
              Left((leftChild, leftValue, rightChild))
            case Right(leftChild) =>
              Right(ThreeNode(leftValue, rightValue, Some((leftChild, middle, right))))
          }
        } getOrElse {
          val leftChild = TwoNode(elem, None)
          val rightChild = TwoNode(rightValue, None)
          Left((leftChild, leftValue, rightChild))
        }
      } else if (elem > rightValue) {
        children map {
          case (left, middle, right) => right.doAdd(elem) match {
            case Left((rightMiddle, value, farRight)) =>
              val leftChild = TwoNode(leftValue, Some((left, middle)))
              val rightChild = TwoNode(value, Some((rightMiddle, farRight)))
              Left((leftChild, rightValue, rightChild))
            case Right(rightChild) =>
              Right(ThreeNode(leftValue, rightValue, Some((left, middle, rightChild))))
          }
        } getOrElse {
          val leftChild = TwoNode(leftValue, None)
          val rightChild = TwoNode(elem, None)
          Left((leftChild, rightValue, rightChild))
        }
      } else if (elem != leftValue && elem != rightValue) {
        children map {
          case (left, middle, right) => middle.doAdd(elem) match {
            case Left((leftMiddle, value, rightMiddle)) =>
              val leftChild = TwoNode(leftValue, Some((left, leftMiddle)))
              val rightChild = TwoNode(rightValue, Some((rightMiddle, right)))
              Left((leftChild, value, rightChild))
            case Right(middleChild) =>
              Right(ThreeNode(leftValue, rightValue, Some((left, middleChild, right))))
          }
        } getOrElse {
          val leftChild = TwoNode(leftValue, None)
          val rightChild = TwoNode(rightValue, None)
          Left((leftChild, elem, rightChild))
        }
      } else {
        Right(this)
      }
    }

    override def contains(elem: T): Boolean = {
      if (elem < leftValue) {
        children exists { _._1.contains(elem) }
      } else if (elem > rightValue) {
        children exists { _._3.contains(elem) }
      } else if (elem != leftValue && elem != rightValue) {
        children exists { _._2.contains(elem) }
      } else {
        true
      }
    }

    override def fold[U](base: U)(combine: (U, T) => U): U = {
      children map {
        case (left, middle, right) =>
          right.fold(combine(middle.fold(combine(left.fold(base)(combine), leftValue))(combine), rightValue))(combine)
      } getOrElse combine(combine(base, leftValue), rightValue)
    }
  }
}
