package com.runbam.util


import collection.immutable.RedBlack

class MyMap[A <% Ordered[A], +B] private(val size: Int, t: RedBlack[A]#Tree[B]) extends RedBlack[A] {
  protected val tree: RedBlack[A]#Tree[B] = if (size == 0) Empty else t

  def this() = this (0, null)

  private def newMap[B](s: Int, t: RedBlack[A]#Tree[B]) = new MyMap[A, B](s, t)

  def isSmaller(x: A, y: A) = x < y

  def +[B1 >: B](key: A, value: B1): MyMap[A, B1] = {
    val newsize = if (tree.lookup(key).isEmpty) size + 1 else size
    newMap(newsize, tree.update(key, value))
  }

  def -(key: A): MyMap[A, B] =
    if (tree.lookup(key).isEmpty) this
    else newMap(size - 1, tree.delete(key))

  def get(key: A): Option[B] = tree.lookup(key) match {
    case n: NonEmpty[B] => Some(n.value)
    case _ => None
  }

  def contains(key: A): Boolean = !tree.lookup(key).isEmpty

  def first: A = tree.first

  def last: A = tree.last
}