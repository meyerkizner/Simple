package com.prealpha.simple

trait SimpleSet[T] {
  def add(elem: T): SimpleSet[T]

  def remove(elem: T): SimpleSet[T]

  def contains(elem: T): Boolean

  def fold[U](combine: T => U => U, base: U): U

  def union(other: SimpleSet[T]): SimpleSet[T] = {
    other.fold({ elem => set => set.add(elem) }, this)
  }

  def intersection(other: SimpleSet[T]): SimpleSet[T] = {
    fold({ elem => set =>
      if (other.contains(elem)) {
        set
      } else {
        set.remove(elem)
      }
    }, this)
  }
}
