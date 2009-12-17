package com.runbam.adt.heap

trait Heap[A] {
  def insert(a:A)
  def extractMin() : Option[A]
  def delete(a:A)
  def minimum : Option[A]
  def decreaseKey(a:A,k:A) 
  def empty : Boolean
  def size: Int
  
  def += (a:A) = insert(a)
  def -= (a:A) = delete(a)
  def ++= (l:List[A]) = l.foreach(+= _)
  def --= (l:List[A]) = l.foreach(-= _)
  
}