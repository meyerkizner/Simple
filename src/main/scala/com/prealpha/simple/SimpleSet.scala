package com.prealpha.simple

trait SimpleSet[T] {
  def add(elem: T): SimpleSet[T]

  def remove(elem: T): SimpleSet[T]

  def contains(elem: T): Boolean

  def fold[U](base: U)(combine: (U, T) => U): U

  def union(other: SimpleSet[T]): SimpleSet[T] = {
    other.fold[SimpleSet[T]](this) { (set, elem) =>
      set.add(elem)
    }
  }

  def intersection(other: SimpleSet[T]): SimpleSet[T] = {
    fold[SimpleSet[T]](this) { (set, elem) =>
      if (other.contains(elem)) {
        set
      } else {
        set.remove(elem)
      }
    }
  }
}
