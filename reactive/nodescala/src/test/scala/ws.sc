import scala.language.postfixOps
import scala.concurrent._
import scala.concurrent.duration._
import Duration._
import nodescala._
import ExecutionContext.Implicits.global

object ws {
    val f1 = Future(5)                            //> f1  : scala.concurrent.Future[Int] = scala.concurrent.impl.Promise$DefaultPr
                                                  //| omise@4cf777e8
    val f2 = Future(6)                            //> f2  : scala.concurrent.Future[Int] = scala.concurrent.impl.Promise$DefaultPr
                                                  //| omise@1e397ed7
    val v = Await.result(Future.all(List(f1,f2)), 1 millis)
                                                  //> v  : <error> = List(5, 6)
}