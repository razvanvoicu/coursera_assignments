package objsets

import common._
import TweetReader._

/**
 * A class to represent tweets.
 */
class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"
}

/**
 * This represents a set of objects of type `Tweet` in the form of a binary search
 * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
 * invariant which always holds: for every branch `b`, all elements in the left
 * subtree are smaller than the tweet at `b`. The elements in the right subtree are
 * larger.
 *
 * Note that the above structure requires us to be able to compare two tweets (we
 * need to be able to say which of two tweets is larger, or if they are equal). In
 * this implementation, the equality / order of tweets is based on the tweet's text
 * (see `def incl`). Hence, a `TweetSet` could not contain two tweets with the same
 * text from different users.
 *
 *
 * The advantage of representing sets as binary search trees is that the elements
 * of the set can be found quickly. If you want to learn more you can take a look
 * at the Wikipedia page [1], but this is not necessary in order to solve this
 * assignment.
 *
 * [1] http://en.wikipedia.org/wiki/Binary_search_tree
 */
abstract class TweetSet {
  
  def getElem: Tweet
  def getLeft: TweetSet
  def getRight: TweetSet

  /**
   * This method takes a predicate and returns a subset of all the elements
   * in the original set for which the predicate is true.
   *
   * Question: Can we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def filter(p: Tweet => Boolean): TweetSet = {
    filterAcc(p,new Empty)
  }

  /**
   * This is a helper method for `filter` that propagetes the accumulated tweets.
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = filterAccs(p,acc,List())
  
  def filterAccs(p:Tweet => Boolean, acc: TweetSet, accs: List[TweetSet]): TweetSet

  /**
   * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
   def union(that: TweetSet) = unions(List(that))
   
   def unions(those: List[TweetSet]): TweetSet

  /**
   * Returns the tweet from this set which has the greatest retweet count.
   *
   * Calling `mostRetweeted` on an empty set should throw an exception of
   * type `java.util.NoSuchElementException`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def mostRetweeted: Tweet 

  /**
   * Returns a list containing all tweets of this set, sorted by retweet count
   * in descending order. In other words, the head of the resulting list should
   * have the highest retweet count.
   *
   * Hint: the method `remove` on TweetSet will be very useful.
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def descendingByRetweet: TweetList 
    

  /**
   * The following methods are already implemented
   */

  /**
   * Returns a new `TweetSet` which contains all elements of this set, and the
   * the new element `tweet` in case it does not already exist in this set.
   *
   * If `this.contains(tweet)`, the current set is returned.
   */
  def incl(tweet: Tweet): TweetSet

  /**
   * Returns a new `TweetSet` which excludes `tweet`.
   */
  def remove(tweet: Tweet): TweetSet

  /**
   * Tests if `tweet` exists in this `TweetSet`.
   */
  def contains(tweet: Tweet): Boolean

  /**
   * This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit
}

class Empty extends TweetSet {

  def getElem: Tweet = throw new NoSuchElementException("getElem called on Empty")
  def getLeft: TweetSet = throw new NoSuchElementException("getLeft called on Empty")
  def getRight: TweetSet = throw new NoSuchElementException("getRight called on Empty")

  def filterAccs(p: Tweet => Boolean, that: TweetSet, those: List[TweetSet]): TweetSet = those match {
    case List() => that
    case head :: tail => head.filterAccs(p,that,tail)    
  }
  
  def unions (those: List[TweetSet]) = those match {
    case List() => this
    case head :: tail => head.unions(tail)
  }

  def mostRetweeted: Tweet = throw new NoSuchElementException("mostRetweeted called on an empty list")
  
  def descendingByRetweet: TweetList = Nil

  /**
   * The following methods are already implemented
   */

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

  def getElem = elem
  def getLeft = left
  def getRight = right
  
  def filterAccs(p: Tweet => Boolean, that: TweetSet, those: List[TweetSet]): TweetSet = {
    def next(x:Tweet): TweetSet = if(p(x)) that.incl(x) else that
    def resultExpr(main: TweetSet, aux: TweetSet, tail: List[TweetSet]): TweetSet = 
        main.filterAccs(p,next(aux.getElem),aux.getRight::tail)
    those match {
      case List() => resultExpr(left,this,List())
      case (head:Empty) :: tail => filterAccs(p,that,tail)
      case (head:NonEmpty) :: tail => resultExpr(this,head,head.getLeft::tail)
      case _ => new Empty      
    }
  }
  
  def unions(those: List[TweetSet]) : TweetSet = {
    those match {
      case List() => this
      case (head:Empty) :: tail => unions(tail)
      case (head:NonEmpty) :: tail => incl(head.getElem).unions(head.getLeft :: head.getRight :: tail)
      case _ => new Empty
    }
  }

  def mostRetweeted:Tweet = {
    def max(curr:Tweet,left:Tweet,right:Tweet): Tweet = {
      if(left.retweets >= right.retweets)
        if(curr.retweets >= left.retweets)
          curr
        else
          left
      else
        if(curr.retweets >= right.retweets)
          curr
        else
          right
    }
    val leftMax:Tweet = left match {
      case _:Empty => new Tweet("","",-1)
      case _ => left.mostRetweeted
    }
    val rightMax:Tweet = right match {
      case _:Empty => new Tweet("","",-1)
      case _ => right.mostRetweeted
    }
    max(elem,leftMax,rightMax)
  }
  
  def descendingByRetweet: TweetList = new Cons(mostRetweeted,remove(mostRetweeted).descendingByRetweet)

  /**
   * The following methods are already implemented
   */

  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }
}

trait TweetList {
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

object Nil extends TweetList {
  
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}


object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  def filterTrending(ts:TweetSet,keywords:List[String]) = ts.filter((t) => keywords.exists((s) => t.text.contains(s)))
  
  lazy val googleTweets: TweetSet = filterTrending(TweetReader.allTweets,google)
  lazy val appleTweets: TweetSet = filterTrending(TweetReader.allTweets,apple)

  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
  lazy val trending: TweetList = filterTrending(TweetReader.allTweets, google ++ apple).descendingByRetweet
}

object Main extends App {
  // Print the trending tweets
  //val t0 = System.nanoTime
  GoogleVsApple.trending foreach println
  //val t1 = System.nanoTime
  //println("time:"+(t1-t0))
}
