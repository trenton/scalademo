package com.runbam.util

class BinarySearchTree {
  // root consists of dummy node. we *always* use the left tree.
  val root = new DummyNode
  var n: Int = 0

  def isEmpty = {n == 0}

  def size = n

  def insert(value: Int): Unit = {
    isEmpty match {
      case true => root.left = Some(new Node(value, None, None))
      case false => insert(value, root.left.get)
    }

    n += 1
  }

  def +=(value: Int) = insert(value)

  private def insert(value: Int, node: Node): Unit = {
    value <= node.value match {
      case true => {
        node.left match {
          case None => node.left = Some(new Node(value, None, None))
          case Some(x) => insert(value, x)
        }
      }
      case false => {
        node.right match {
          case None => node.right = Some(new Node(value, None, None))
          case Some(x) => insert(value, x)
        }
      }
    }
  }

  def delete(value: Int): Unit = {
    val result = search(value, root.left, root)
    val nodeOpt = result._1
    val parent = result._2

    nodeOpt match {
      case None => ()
      case Some(node) => {
        delete(node, parent)
        n -= 1
      }
    }
  }

  def -=(value: Int) = delete(value)

  private def delete(deleteMe: Node, parent: Node): Unit = {
    deleteMe.degree match {
      case 0 => {
        // find from the parent node which side this node is on
        // and delete is by saying this child is now None
        parent.left match {
          case Some(x) if x == deleteMe => parent.left = None
          case _ => parent.right = None
        }
      }
      case 1 => {
        // find from the parent node which side this node is on and
        // "delete" this node by promoting its 1 leaf to its parent
        parent.left match {
          case Some(x) if x == deleteMe => parent.left = deleteMe.left.orElse(deleteMe.right)
          case _ => parent.right = deleteMe.left.orElse(deleteMe.right)
        }
      }
      case 2 => {
        // :TODO: consider switching between in-order predecessor for better
        // :TODO: runtime characteristics
        n % 2 == 0 match {
          case true => doDeleteWithSuccessor(deleteMe, parent)
          case false => doDeleteWithPredecessor(deleteMe, parent)
        }
      }
    }
  }


  private def doDeleteWithSuccessor(deleteMe: Node, parent: Node) {
    // in-order successor: left (min) most child of right subtree
    // will always have 0 or 1 child, since it will be a leaf and
    // cannot have a left child (or *that* would be the min)
    val findResult = findMin(deleteMe.right.get, deleteMe)

    val replacement = findResult._1
    val replacementParent = findResult._2

    // we don't know if deleteMe is the left or right child, so this finds it
    parent.left match {
      case Some(x) if x == deleteMe => parent.left = Some(replacement)
      case _ => parent.right = Some(replacement)
    }

    // promote minNode into the place of the deleted one

    // replace the deleted node D by setting D's parent to min node's
    // right subtree... unless the parent of the deleted node is the min
    // node's parent, in which case we need only copy its left subtree
    replacementParent == deleteMe match {
      case true => {
        replacement.left = deleteMe.left
      }
      case false => {
        replacement.left = deleteMe.left
        replacementParent.left = replacement.right
        replacement.right = deleteMe.right
      }
    }
  }

  /**
   * See doDeleteWithSuccessor for general replacement strategy. This is
   * much the mirror image -- replace the deleted node's parent RIGHT
   * subtree with MAX node in the deleted node's LEFT subtree
   */
  private def doDeleteWithPredecessor(deleteMe: Node, parent: Node) {
    // in-order successor: left (min) most child of right subtree
    // will always have 0 or 1 child, since it will be a leaf and
    // cannot have a left child (or *that* would be the min)
    val findResult = findMax(deleteMe.left.get, deleteMe)

    val replacement = findResult._1
    val replacementParent = findResult._2

    // we don't know if deleteMe is the left or right child, so this finds it
    parent.left match {
      case Some(x) if x == deleteMe => parent.left = Some(replacement)
      case _ => parent.right = Some(replacement)
    }

    replacementParent == deleteMe match {
      case true => {
        replacement.right = deleteMe.right
      }
      case false => {
        replacement.right = deleteMe.right
        replacementParent.right = replacement.left
        replacement.left = deleteMe.left
      }
    }
  }

  def search(value: Int): Boolean = {
    (search(value, root.left, root))._1.isDefined
  }

  /**
   * tuple: (node matching the search, node of that parent)
   */
  def search(value: Int, nodeOpt: Option[Node], parent: Node): (Option[Node], Node) = nodeOpt match {
    case None => (None, parent)
    case Some(node) => {
      value.compare(node.value) match {
        case -1 => search(value, node.left, node)
        case 1 => search(value, node.right, node)
        case 0 => (nodeOpt, parent)
      }
    }
  }

  def findMin: Int = {
    root.left match {
      case None => throw new NoSuchElementException("Tree is empty")
      case Some(node) => findMin(node, root)._1.value
    }
  }

  /**(node containing smallest, its parent) */
  def findMin(node: Node, parent: Node): (Node, Node) = {
    node.left match {
      case None => (node, parent)
      case Some(child) => findMin(child, node)
    }
  }

  def findMax: Int = {
    // we always use root's left node, since it's a dummy
    root.left match {
      case None => throw new NoSuchElementException("Tree is empty")
      case Some(node) => findMax(node, root)._1.value
    }
  }

  def findMax(node: Node, parent: Node): (Node, Node) = {
    node.right match {
      case None => (node, parent)
      case Some(child) => findMax(child, node)
    }
  }

  def iterate: List[Int] = {
    iterate(root.left)
  }

  private def iterate(node: Option[Node]): List[Int] = {
    node match {
      case None => Nil
      case Some(x) => iterate(x.left) ::: (x.value :: Nil) ::: iterate(x.right)
    }
  }
}

object BinarySearchTree {
  def empty = new BinarySearchTree()
}

class Node private() {
  private var xleft: Option[Node] = None
  private var xright: Option[Node] = None
  private var xvalue: Int = 0

  def this(value: Int, left: Option[Node], right: Option[Node]) = {
    this ()
    value_=(value)
    left_=(left)
    right_=(right)
  }

  private def value_=(v: Int) = {xvalue = v}

  def value = xvalue

  def right_=(n: Option[Node]) = {xright = n}

  def right = xright

  def left_=(n: Option[Node]) = {xleft = n}

  def left = xleft

  def degree: Int = {
    left == None match {
      case true if right == None => 0
      case false if right != None => 2
      case _ => 1
    }
  }
}

sealed class DummyNode() extends Node(0, None, None) {
  override def value: Int = {
    throw new RuntimeException("Reading field 'value' from DummyNode is bunk")
  }
}