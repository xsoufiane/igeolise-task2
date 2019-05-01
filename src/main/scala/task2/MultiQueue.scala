package task2

import java.lang.{Boolean => JBoolean}
import java.util.concurrent.atomic.{AtomicInteger, AtomicReferenceArray}

import scala.util.Random
import scala.util.control.TailCalls._

class MultiQueue[A >: Null] private (nQueues: Int)(implicit ord: Ordering[A]) {
  private[this] val queues = Vector.fill(nQueues)(Queue.empty[A])
  private[this] val references: AtomicReferenceArray[JBoolean] = new AtomicReferenceArray(Array.fill(nQueues)(JBoolean.FALSE))
  private[this] val random = Random

  val size = new AtomicInteger()

  def isEmpty: Boolean = size.get() == 0

  def insert(element: A): Unit = {
    @scala.annotation.tailrec
    def helper(): Unit = {
      val i = random.nextInt(nQueues)

      references.compareAndSet(i, false, true) match {
        case true => {
          queues(i).enqueue(element)
          references.set(i, false)
          size.incrementAndGet()
          ()
        }
        case false => helper()
      }
    }

    helper()
  }

  def deleteMin(): A = {

    def helper(): TailRec[A] = {

      def lock(i: Int): TailRec[A] = references.compareAndSet(i, false, true) match {
        case true => {
          val min = queues(i).deleteMin()
          references.set(i, false)
          size.decrementAndGet()
          done(min)
        }
        case false => tailcall(helper())
      }

      val i = random.nextInt(nQueues)
      val j = random.nextInt(nQueues)

      if (queues(i).isEmpty || queues(j).isEmpty) {
        helper()
      }
      else {
        ord.lteq(queues(i).peekMin, queues(j).peekMin) match {
          case true => lock(i)
          case false => lock(j)
        }
      }
    }

    if(isEmpty) null else helper().result
  }
}

object MultiQueue {

  def empty[A >: Null](nQueues: Int = 2)(implicit ord: Ordering[A]): MultiQueue[A] = {
    new MultiQueue(math.max(2, nQueues))
  }
}
