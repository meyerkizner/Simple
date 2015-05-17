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

  protected def doRemove(elem: T): Either[RedBlackTree.BlackNode[T], RedBlackTree[T]]

  override final def remove(elem: T): RedBlackTree[T] = doRemove(elem).merge

  protected def max: Option[T]
}

object RedBlackTree {
  import scala.math.Ordering.Implicits._

  protected sealed abstract class BlackNode[T: Ordering] extends RedBlackTree[T] {
    override protected[RedBlackTree] def doAdd(elem: T): Right[InvalidRed, RedBlackTree[T]]

    override protected[RedBlackTree] def doRemove(elem: T): Either[BlackNode[T], BlackNode[T]]
  }

  private case class Empty[T: Ordering]() extends BlackNode[T] {
    override protected[RedBlackTree] def doAdd(elem: T): Right[InvalidRed, RedBlackTree[T]] = {
      Right(RedNode(this, elem, this))
    }

    override protected[RedBlackTree] def doRemove(elem: T): Either[BlackNode[T], BlackNode[T]] = Right(this)

    override def contains(elem: T): Boolean = false

    override def fold[U](base: U)(combine: (U, T) => U): U = base

    override protected[RedBlackTree] def max: Option[T] = None
  }

  private case class BlackNonEmpty[T: Ordering](
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

    override protected[RedBlackTree] def doRemove(elem: T): Either[BlackNode[T], BlackNode[T]] = {
      if (elem <= value) {
        val anchor = if (elem == value) left.max else Some(value)
        anchor map { anchorValue =>
          val toRemove = if (elem == value) anchorValue else elem
          (left.doRemove(toRemove), right) match {
            case (Left(_), Empty()) | (Left(_), RedNode(Empty(), _, _)) | (Left(_), RedNode(_, _, Empty())) =>
              throw new IllegalStateException("unbalanced RedBlackTree")
            case (Left(orphan), RedNode(BlackNonEmpty(leftMiddle: BlackNode[T], middleValue, rightMiddle: BlackNode[T]), rightValue, farRight)) =>
              val leftChild = BlackNonEmpty(orphan, anchorValue, RedNode(leftMiddle, middleValue, rightMiddle))
              Right(BlackNonEmpty(leftChild, rightValue, farRight))
            case (Left(orphan), RedNode(BlackNonEmpty(RedNode(farLeft, leftValue, leftMiddle), middleValue, rightMiddle: BlackNode[T]), rightValue, farRight)) =>
              val farLeftChild = BlackNonEmpty(orphan, anchorValue, farLeft)
              val leftMiddleChild = BlackNonEmpty(leftMiddle, middleValue, rightMiddle)
              val leftChild = BlackNonEmpty(farLeftChild, leftValue, leftMiddleChild)
              Right(BlackNonEmpty(leftChild, rightValue, farRight))
            case (Left(orphan), RedNode(BlackNonEmpty(farLeft, leftValue, RedNode(leftMiddle, middleValue, rightMiddle)), rightValue, farRight)) =>
              val farLeftChild = BlackNonEmpty(orphan, anchorValue, farLeft)
              val leftMiddleChild = BlackNonEmpty(leftMiddle, middleValue, rightMiddle)
              val leftChild = BlackNonEmpty(farLeftChild, leftValue, leftMiddleChild)
              Right(BlackNonEmpty(leftChild, rightValue, farRight))
            case (Left(orphan), BlackNonEmpty(rightMiddle: BlackNode[T], rightValue, farRight: BlackNode[T])) =>
              val rightChild = RedNode(rightMiddle, rightValue, farRight)
              Left(BlackNonEmpty(orphan, anchorValue, rightChild))
            case (Left(orphan), BlackNonEmpty(RedNode(leftMiddle, middleValue, rightMiddle), rightValue, farRight: BlackNode[T])) =>
              val leftChild = BlackNonEmpty(orphan, anchorValue, leftMiddle)
              val rightChild = BlackNonEmpty(rightMiddle, rightValue, farRight)
              Right(BlackNonEmpty(leftChild, middleValue, rightChild))
            case (Left(orphan), BlackNonEmpty(leftMiddle, middleValue, RedNode(rightMiddle, rightValue, farRight))) =>
              val leftChild = BlackNonEmpty(orphan, anchorValue, leftMiddle)
              val rightChild = BlackNonEmpty(rightMiddle, rightValue, farRight)
              Right(BlackNonEmpty(leftChild, middleValue, rightChild))
            case (Right(leftChild), rightChild) =>
              Right(BlackNonEmpty(leftChild, anchorValue, rightChild))
          }
        } getOrElse {
          right match {
            case RedNode(leftChild, newValue, rightChild) =>
              Right(BlackNonEmpty(leftChild, newValue, rightChild))
            case newTree: BlackNode[T] =>
              Left(newTree)
          }
        }
      } else {
        (left, right.doRemove(elem)) match {
          case (Empty(), Left(_)) | (RedNode(Empty(), _, _), Left(_)) | (RedNode(_, _, Empty()), Left(_)) =>
            throw new IllegalStateException("unbalanced RedBlackTree")
          case (RedNode(farLeft, leftValue, BlackNonEmpty(leftMiddle: BlackNode[T], middleValue, rightMiddle: BlackNode[T])), Left(orphan)) =>
            val rightChild = BlackNonEmpty(RedNode(leftMiddle, middleValue, rightMiddle), value, orphan)
            Right(BlackNonEmpty(farLeft, leftValue, rightChild))
          case (RedNode(farLeft, leftValue, BlackNonEmpty(leftMiddle: BlackNode[T], middleValue, RedNode(rightMiddle, rightValue, farRight))), Left(orphan)) =>
            val rightMiddleChild = BlackNonEmpty(leftMiddle, middleValue, rightMiddle)
            val farRightChild = BlackNonEmpty(farRight, value, orphan)
            val rightChild = BlackNonEmpty(rightMiddleChild, rightValue, farRightChild)
            Right(BlackNonEmpty(farLeft, leftValue, rightChild))
          case (RedNode(farLeft, leftValue, BlackNonEmpty(RedNode(leftMiddle, middleValue, rightMiddle), rightValue, farRight)), Left(orphan)) =>
            val rightMiddleChild = BlackNonEmpty(leftMiddle, middleValue, rightMiddle)
            val farRightChild = BlackNonEmpty(farRight, value, orphan)
            val rightChild = BlackNonEmpty(rightMiddleChild, rightValue, farRightChild)
            Right(BlackNonEmpty(farLeft, leftValue, rightChild))
          case (BlackNonEmpty(farLeft: BlackNode[T], leftValue, leftMiddle: BlackNode[T]), Left(orphan)) =>
            val leftChild = RedNode(farLeft, leftValue, leftMiddle)
            Left(BlackNonEmpty(leftChild, value, orphan))
          case (BlackNonEmpty(farLeft: BlackNode[T], leftValue, RedNode(leftMiddle, middleValue, rightMiddle)), Left(orphan)) =>
            val leftChild = BlackNonEmpty(farLeft, leftValue, leftMiddle)
            val rightChild = BlackNonEmpty(rightMiddle, value, orphan)
            Right(BlackNonEmpty(leftChild, middleValue, rightChild))
          case (BlackNonEmpty(RedNode(farLeft, leftValue, leftMiddle), middleValue, rightMiddle), Left(orphan)) =>
            val leftChild = BlackNonEmpty(farLeft, leftValue, leftMiddle)
            val rightChild = BlackNonEmpty(rightMiddle, value, orphan)
            Right(BlackNonEmpty(leftChild, middleValue, rightChild))
          case (leftChild, Right(rightChild)) =>
            Right(BlackNonEmpty(leftChild, value, rightChild))
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

    override protected[RedBlackTree] def max: Option[T] = right.max match {
      case None => Some(value)
      case Some(max) => Some(max)
    }
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

    override protected def doRemove(elem: T): Either[BlackNode[T], RedBlackTree[T]] = {
      if (elem <= value) {
        val anchor = if (elem == value) left.max else Some(value)
        anchor map { anchorValue =>
          val toRemove = if (elem == value) anchorValue else elem
          (left.doRemove(toRemove), right) match {
            case (Left(_), Empty()) =>
              throw new IllegalStateException("unbalanced RedBlackTree")
            case (Left(orphan), BlackNonEmpty(rightMiddle: BlackNode[T], rightValue, farRight: BlackNode[T])) =>
              val rightChild = RedNode(rightMiddle, rightValue, farRight)
              Right(BlackNonEmpty(orphan, anchorValue, rightChild))
            case (Left(orphan), BlackNonEmpty(RedNode(leftMiddle, middleValue, rightMiddle), rightValue, farRight: BlackNode[T])) =>
              val leftChild = BlackNonEmpty(orphan, anchorValue, leftMiddle)
              val rightChild = BlackNonEmpty(rightMiddle, rightValue, farRight)
              Right(RedNode(leftChild, middleValue, rightChild))
            case (Left(orphan), BlackNonEmpty(leftMiddle, middleValue, RedNode(rightMiddle, rightValue, farRight))) =>
              val leftChild = BlackNonEmpty(orphan, anchorValue, leftMiddle)
              val rightChild = BlackNonEmpty(rightMiddle, rightValue, farRight)
              Right(RedNode(leftChild, middleValue, rightChild))
            case (Right(leftChild), rightChild) =>
              Right(RedNode(leftChild, anchorValue, rightChild))
          }
        } getOrElse {
          Right(right)
        }
      } else {
        (left, right.doRemove(elem)) match {
          case (Empty(), Left(_)) =>
            throw new IllegalStateException("unbalanced RedBlackTree")
          case (BlackNonEmpty(farLeft: BlackNode[T], leftValue, leftMiddle: BlackNode[T]), Left(orphan)) =>
            val leftChild = RedNode(farLeft, leftValue, leftMiddle)
            Right(BlackNonEmpty(leftChild, value, orphan))
          case (BlackNonEmpty(farLeft: BlackNode[T], leftValue, RedNode(leftMiddle, middleValue, rightMiddle)), Left(orphan)) =>
            val leftChild = BlackNonEmpty(farLeft, leftValue, leftMiddle)
            val rightChild = BlackNonEmpty(rightMiddle, value, orphan)
            Right(RedNode(leftChild, middleValue, rightChild))
          case (BlackNonEmpty(RedNode(farLeft, leftValue, leftMiddle), middleValue, rightMiddle), Left(orphan)) =>
            val leftChild = BlackNonEmpty(farLeft, leftValue, leftMiddle)
            val rightChild = BlackNonEmpty(rightMiddle, value, orphan)
            Right(RedNode(leftChild, middleValue, rightChild))
          case (leftChild, Right(rightChild)) =>
            Right(RedNode(leftChild, value, rightChild))
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
}
