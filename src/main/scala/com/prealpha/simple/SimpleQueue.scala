package com.prealpha.simple

trait SimpleQueue[T] {
  def enqueue(elem: T): SimpleQueue[T]

  def dequeue: SimpleQueue[T]

  def peek: Option[T]
}
