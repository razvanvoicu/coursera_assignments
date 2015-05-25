package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.util.Random.shuffle

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  def makeHeapsFromList (l:List[Int]) : (H,H) = {
    def makeHeap : List[Int] => H = _.foldRight(empty)(insert)
    ( makeHeap(l), makeHeap(l.reverse) )
  }

  def deleteMins : ((H,H)) => (H,H) = { 
      case (h1,h2) => (deleteMin(h1), deleteMin(h2)) 
  }
  /*
  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { h : H =>
    val withMinInt = insert(Int.MinValue, h)
    findMin(withMinInt) == Int.MinValue
  }

  property("min3") = forAll { h : H =>
    (! isEmpty(h)) ==> {
      val withMaxInt = insert(Int.MaxValue,h)
      findMin(withMaxInt) == findMin(h)
    }
  }

  property("min4") = forAll { l : List[Int] =>
    (l.length > 0) ==> {
      val (h1,h2) = makeHeapsFromList(l)
      findMin(h1) == findMin(h2)
    }
  }
  */
  property("del_min") = forAll { l : List[Int] =>
    (l.length > 1) ==> {
      val (h1,h2) = deleteMins(makeHeapsFromList(l))
      findMin(h1) == findMin(h2)
    }
  }
  /*
  property("min_meld") = forAll { (h1:H,h2:H) =>
    (! isEmpty(h1) && ! isEmpty(h2)) ==> {
      findMin(meld(h1,h2)) == findMin(meld(h2,h1))
    }
  }

  property("min_del_meld") = forAll { (h1:H,h2:H) =>
    (! isEmpty(h1) && ! isEmpty(h2)) ==> {
      findMin(deleteMin(meld(h1,h2))) == findMin(deleteMin(meld(h2,h1)))
    }
  }

  property("min_del_meld_meld") = forAll { (h1:H,h2:H) =>
    (! isEmpty(h1) && ! isEmpty(h2)) ==> {
      findMin(deleteMin(meld(h1,meld(h1,h2)))) == findMin(deleteMin(meld(h2,meld(h1,h1))))
    }
  }

  property("del_min_meld_reverse") = forAll { (l1 : List[Int], l2: List[Int]) =>
    (l1.length > 1 && l2.length > 1) ==> {
      val (h1,h1r) = deleteMins(makeHeapsFromList(l1))
      val (h2,h2r) = deleteMins(makeHeapsFromList(l2))
      findMin(deleteMin(meld(h1,h2))) == findMin(deleteMin(meld(h2r,h1r)))
    }
  }
  */
  lazy val genHeap: Gen[H] = for {
    size <- Gen.choose(0,100)
  }  yield (shuffle(1 to size.abs)).foldRight(empty)(insert)
 

  implicit lazy val arbHeap: Arbitrary[H] =  Arbitrary(genHeap)

}
