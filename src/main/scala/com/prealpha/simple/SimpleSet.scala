package com.prealpha.simple

trait SimpleSet[T] {
  def add(elem: T): SimpleSet[T]

  def remove(elem: T): SimpleSet[T]

  def contains(elem: T): Boolean

  def fold[U](combine: (T, U) => U, base: U): U

  def union(other: SimpleSet[T]): SimpleSet[T] = {
    other.fold[SimpleSet[T]]({ (elem, set) => set.add(elem) }, this)
  }

  def intersection(other: SimpleSet[T]): SimpleSet[T] = {
    fold[SimpleSet[T]]({ (elem, set) =>
      if (other.contains(elem)) {
        set
      } else {
        set.remove(elem)
      }
    }, this)
  }
}
