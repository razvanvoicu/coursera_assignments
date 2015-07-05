package kvstore

import akka.actor.Props
import akka.actor.Actor
import akka.actor.ActorRef
import scala.concurrent.duration._

object Replicator {
  case class Replicate(key: String, valueOption: Option[String], id: Long)
  case class Replicated(key: String, id: Long)
  
  case class Snapshot(key: String, valueOption: Option[String], seq: Long)
  case class SnapshotAck(key: String, seq: Long)
  
  case object RetryReplication

  def props(replica: ActorRef): Props = Props(new Replicator(replica))
}

class Replicator(val replica: ActorRef) extends Actor {
  import Replicator._
  import Replica._
  import context.dispatcher
  
  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */

  // map from sequence number to pair of sender and request
  var acks = Map.empty[Long, (Replicated,Snapshot)]
  // a sequence of not-yet-sent snapshots (you can disregard this if not implementing batching)
  var pending = Vector.empty[Snapshot]
  
  var _seqCounter = 0L
  def nextSeq = {
    val ret = _seqCounter
    _seqCounter += 1
    ret
  }

  var primary : ActorRef = null
  
  /* TODO Behavior for the Replicator. */
  def receive: Receive = {
    case Replicate(key,valueOption,id) => {
      primary = sender
      val seq = nextSeq
      val ack = Replicated(key,id)
      val msg = Snapshot(key,valueOption,seq)
      acks += seq -> (ack,msg)
      replica ! msg
    }
    case SnapshotAck(key,seq) => if(acks.contains(seq)) {
      primary ! acks(seq)._1
      acks -= seq
    }
    case RetryReplication => {
      acks.foreach(replica ! _._2._2)
    }
  }

  context.system.scheduler.schedule(100.millis,100.millis,context.self,RetryReplication)
}
