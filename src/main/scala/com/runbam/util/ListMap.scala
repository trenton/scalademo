package com.runbam.util

import collection.immutable.TreeMap

class ListMap[K <% Ordered[K], +V] private(store: TreeMap[K, List[V]]) extends scala.collection.Map[K, List[V]] {
  def this() = this (TreeMap.empty)

  def size = store.size

  def elements = store.elements

  def get(key: K): Option[List[V]] = store.get(key)

  /**
   * If key exists, appends value to its list. If key does not exist, creates a
   * list with the 1 value passed.
   */
  def +[V1 >: V](key: K, value: V1): ListMap[K, V1] = {
    get(key) match {
      case Some(xs: List[V]) => new ListMap(store.update(key, xs ::: List(value)))
      case None => new ListMap(store.update(key, List(value)))
    }
  }


  /**
   * Removes the key and all values from this Map.
   */
  def -(key: K): ListMap[K, V] = {
    new ListMap(store - key)
  }

}
