/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import scala.collection.immutable.Queue

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation
  
  /** Request to perform garbage collection*/
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply
  
  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}


class BinaryTreeSet extends Actor {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  // optional
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = { 
    case o:Operation => root ! o 
    case GC => {
      val newRoot = context.actorOf(BinaryTreeNode.props(0,initiallyRemoved=true))
      root ! CopyTo(newRoot)
      context.become(garbageCollecting(newRoot))
    }
  }

  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = {
    case o:Operation => {
      pendingQueue = pendingQueue.enqueue(o)
    }
    case CopyFinished => {
      pendingQueue.foreach(newRoot ! _)
      pendingQueue = Queue.empty[Operation]
      root ! PoisonPill
      root = newRoot
      context.unbecome()
    }
  }

}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved
  var parent : ActorRef = null

  // optional
  def receive = normal

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = { 
    case o:Insert => insertElem(o)
    case o:Contains => containsElem(o)
    case o:Remove => removeElem(o)
    case CopyTo(node) => copyTo(node)
  }

  def sendReply(requester:ActorRef,reply:OperationReply,action: => Unit) = {
    action
    requester ! reply
  }
  
  def traverseSubtree ( p:Position, o:Operation, requester:ActorRef, action: => Unit, reply: OperationReply): Unit = {
        if(subtrees contains p)
          subtrees(p) ! o
        else 
          sendReply(requester,reply,action)
  }
  
  def insertInto(p:Position,o:Insert): Unit = o match {
    case Insert(requester,id,newElem) => {
      def action = {
        val newNode = context.actorOf(BinaryTreeNode.props(newElem, initiallyRemoved = false))
        subtrees += (p -> newNode)
      }
      traverseSubtree(p,o,requester,action,OperationFinished(id))
    }
  }
  
  val insertElem : Insert => Unit = {
      case o @ Insert(_,_,newElem) if newElem < elem => insertInto(Left,o)
      case o @ Insert(_,_,newElem) if newElem > elem => insertInto(Right,o)
      case Insert(requester,id,_) => sendReply(requester,OperationFinished(id),{removed=false})
    }
  
    def searchSubtree(p:Position,o:Contains): Unit = o match {
      case Contains(requester,id,newElem) => traverseSubtree(p,o,requester,(),ContainsResult(id,false))
    }
  
    val containsElem : Contains => Unit =  {
      case o @ Contains(_,_,queryElem) if queryElem < elem => searchSubtree(Left,o)
      case o @ Contains(_,_,queryElem) if queryElem > elem => searchSubtree(Right,o)
      case Contains(requester,id,_) => sendReply(requester,ContainsResult(id,!removed),())
    }
    
    def removeFromSubtree(p:Position,o:Remove) = o match {
      case Remove(requester,id,_) => traverseSubtree(p,o,requester,(),OperationFinished(id))
    }
    
    val removeElem : Remove => Unit = { 
      case o @ Remove(_,_,elemToRemove) if elemToRemove < elem => removeFromSubtree(Left,o)
      case o @ Remove(_,_,elemToRemove) if elemToRemove > elem => removeFromSubtree(Right,o)
      case Remove(requester,id,_) => sendReply(requester,OperationFinished(id),{removed=true})
    }

    def copyTo(node:ActorRef) = {
      if(subtrees.contains(Left)) subtrees(Left) ! CopyTo(node)
      if(subtrees.contains(Right)) subtrees(Right) ! CopyTo(node)
      if (!removed) node ! Insert(self,0,elem)
      parent = sender()
      context.become(copying(subtrees.values.toSet,removed))
    }
    
    // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = {
    if(expected.isEmpty && insertConfirmed) {
      { parent ! CopyFinished }
      { case _ => () }
    } else {
      case CopyFinished => {
        context.become(copying(expected - expected.head,insertConfirmed))
      }
      case OperationFinished(_) => context.become(copying(expected,true))
    }
  }
}
