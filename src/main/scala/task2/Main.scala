package task2

import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._

object Main extends App {

  val multiQueue = MultiQueue.empty[Integer]()

  val elements = (1 to 600).par

  def f1: Future[Unit] = Future { elements.foreach(multiQueue.insert(_)) }
  def f2: Future[Unit] = Future { elements.foreach{ _ =>
    multiQueue.deleteMin()
    ()
  }}

  val f = for {
    _ <- f1
    _ <- f2
  } yield ()

  Await.result(f, 10000.seconds)

  println(multiQueue.size)
}
