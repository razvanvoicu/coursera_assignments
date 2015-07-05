package kvstore

import akka.actor.{ OneForOneStrategy, Props, ActorRef, Actor }
import kvstore.Arbiter._
import scala.collection.immutable.Queue
import akka.actor.SupervisorStrategy.Restart
import scala.annotation.tailrec
import akka.pattern.{ ask, pipe }
import akka.actor.Terminated
import scala.concurrent.duration._
import akka.actor.PoisonPill
import akka.actor.OneForOneStrategy
import akka.actor.SupervisorStrategy
import akka.util.Timeout

object Replica {
  sealed trait Operation {
    def key: String
    def id: Long
  }
  case class Insert(key: String, value: String, id: Long) extends Operation
  case class Remove(key: String, id: Long) extends Operation
  case class Get(key: String, id: Long) extends Operation

  sealed trait OperationReply
  case class OperationAck(id: Long) extends OperationReply
  case class OperationFailed(id: Long) extends OperationReply
  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply

  case class PersistenceTimeout(id: Long)
  case class PersistenceRetry(id: Long)
  
  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(new Replica(arbiter, persistenceProps))
}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor {
  import Replica._
  import Replicator._
  import Persistence._
  import context.dispatcher

  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */
  
  val NO_ID = -1L
  
  var kv = Map.empty[String, String]
  // a map from secondary replicas to replicators
  var secondaries = Map.empty[ActorRef, ActorRef]
  // the current set of replicators
  var replicators = Set.empty[ActorRef]
  // The current persistence actor attached to the replica
  var persister = context.actorOf(persistenceProps)
  // The senders and the operations for which ack/persistence is pending
  var senders = Map.empty[Long,(ActorRef,Operation)]
  // The replicators that yet to confirm replication of a msg id
  var replicationPending = Map.empty[Long,Set[ActorRef]]
  // The ids for which persistence is yet to be confirmed
  var primaryPersistencePending = Set.empty[Long]
  
  def killReplicaAndReplicator(replica:ActorRef):Unit = {
    replica ! PoisonPill
    secondaries(replica) ! PoisonPill
  }
  
  def killObsoleteSecondaries(actualSecondaries:Set[ActorRef]):Map[ActorRef,ActorRef] = {
    val secsToKill = secondaries.keySet.diff(actualSecondaries)
    secsToKill.foreach(killReplicaAndReplicator)
    secondaries.filterKeys(!secsToKill.contains(_))    
  }
  
  def addNewReplicaToSecondaries(newReplica:ActorRef):Unit = {
    secondaries += newReplica -> context.actorOf(Replicator.props(newReplica))
  }
  
  def replicateCurrentKVPairsAtNewReplicas(newReplicas:Set[ActorRef]):Unit = {
    def replicateKVPair(replica:ActorRef,key:String,value:String) = secondaries(replica) ! Replicate(key,Some(value),NO_ID)
    newReplicas.foreach(repl => kv.foreach(pair => replicateKVPair(repl,pair._1,pair._2)))    
  }

  def updateReplicationPending() = {
    val newReplicationPending = replicationPending.mapValues(_.intersect(replicators)).filterNot(_._2.isEmpty)
    (replicationPending.keySet.diff(newReplicationPending.keySet)).foreach(self ! Replicated(null,_))
    newReplicationPending
  }
  
  def operationToPersist(id:Long) : Persist = {
    senders(id)._2 match {
      case Insert(key,value,id) => Persist(key,Some(value),id)
      case Remove(key,id) => Persist(key,None,id)
      case _ => throw new Exception("Unexpected operation")
    }
  }
  
  // Update all data pertaining to current secondaries upon receiving a "Replicas" msg
  def updateSecondaries(replicas : Set[ActorRef]) = {
    val actualSecondaries = replicas - self
    val newSecs = actualSecondaries.diff(secondaries.keySet)
    secondaries = killObsoleteSecondaries(actualSecondaries)
    newSecs.foreach(addNewReplicaToSecondaries) // updates secondaries as side effect
    replicators = secondaries.values.toSet
    replicateCurrentKVPairsAtNewReplicas(newSecs)
    replicationPending = updateReplicationPending
  }

  def scheduleRetryAndFailure(id:Long):Unit = {
      (1 to 9).foreach { i:Int => context.system.scheduler.scheduleOnce(100.millis * i, self, PersistenceRetry(id)) }
      context.system.scheduler.scheduleOnce(1.seconds, self, PersistenceTimeout(id))
  }
  
  def canAck(id:Long):Boolean = !replicationPending.contains(id) && !primaryPersistencePending.contains(id)
  
  def updateReplicationPendingUponReplicated(id:Long):Unit = {
    val newPending = 
      if ( replicationPending.contains(id) ) 
        replicationPending(id) - sender 
      else 
        Set.empty[ActorRef]
    if(newPending.isEmpty)
      replicationPending -= id
    else
      replicationPending += id -> newPending
  }
  
  def updatePendingState(id:Long,op:Operation):Unit = {
    scheduleRetryAndFailure(id)
    senders += id -> (sender,op)
    primaryPersistencePending += id
    if(!replicators.isEmpty) replicationPending += id -> replicators
  }
  
  def receive = {
    case JoinedPrimary   => context.become(leader)
    case JoinedSecondary => context.become(replica)
  }

  /* TODO Behavior for  the leader role. */
  val leader: Receive = {
    case Persisted(_,id) => {
      primaryPersistencePending -= id
      if(!replicationPending.contains(id)) {
        senders(id)._1 ! OperationAck(id)
        senders -= id
      } 
    }
    case PersistenceRetry(id) => {
      if(primaryPersistencePending.contains(id)) {
        persister ! operationToPersist(id)
      }
    }
    case PersistenceTimeout(id) => {
      if(!canAck(id)) {
        senders(id)._1 ! OperationFailed(id)
        senders -= id
        replicationPending -= id
        primaryPersistencePending -= id
      }
    }
    case Replicated(key,id) => if (id != NO_ID) {
      updateReplicationPendingUponReplicated(id)
      if(canAck(id)) {
        senders(id)._1 ! OperationAck(id)
        senders -= id        
      }
    }
    case Get(key,id) => sender ! GetResult(key,kv.get(key),id)
    case op@Insert(key,value,id) => {
      kv += key -> value
      persister ! Persist(key,Some(value),id)
      replicators.foreach(_ ! Replicate(key,Some(value),id))
      updatePendingState(id,op)
    }
    case op@Remove(key,id) => {
      kv -= key
      persister ! Persist(key,None,id)
      replicators.foreach(_ ! Replicate(key,None,id))
      updatePendingState(id,op)
    }
    case Replicas(replicas) => updateSecondaries(replicas)
  }
  
  var lastSeq = -1L;
  var persistencePending = Map.empty[Long,Snapshot]
  var replicator : ActorRef = null

  /* TODO Behavior for the replica role. */
  val replica: Receive = {
    case Get(key,id) => sender ! GetResult(key,kv.get(key),id)
    case op@Snapshot(key,valueOption, seq) => { 
      replicator = sender
      seq match {
        case _ if seq == lastSeq + 1 => {
          valueOption match {
            case Some(v) => kv += key -> v
            case None => kv -= key                     
          }
          persister ! Persist(key,valueOption,seq)
          persistencePending += seq -> op
          scheduleRetryAndFailure(seq)
        }
        case _ if seq <= lastSeq => sender ! SnapshotAck(key,seq)
        case _ => ()
      }
    }
    case Persisted(key,seq) => {
      lastSeq = seq
      replicator ! SnapshotAck(key,seq)
      persistencePending -= seq
    }
    case PersistenceRetry(seq) => {
      if(persistencePending.contains(seq)) {
        persistencePending(seq) match {
          case Snapshot(key,valueOption,_) => persister ! Persist(key,valueOption,seq)
        }
      }
    }
    case PersistenceTimeout(seq) => persistencePending -= seq
  }
  
  arbiter ! Join

}

