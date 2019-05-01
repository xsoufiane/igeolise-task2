package task2

import org.scalatest.{FreeSpec, MustMatchers}

class PriorityQueueTest extends FreeSpec with MustMatchers {

  trait Queue[A >: Null] {
    def emptyQueue(implicit ord: Ordering[A]): task2.Queue[A] = Queue.empty[A]
  }

  "A priority queue of integers" - {
    "when empty" - {
      "must be of size 0  " in {
        new Queue[Integer] {
          emptyQueue must have size 0
        }
      }

      "must be empty" in {
        new Queue[Integer] {
          emptyQueue mustBe empty
        }
      }

      "must be null when peekMin and deleteMin is invoked" in {
        new Queue[Integer] {
          val ops: List[Integer] = List(emptyQueue.peekMin, emptyQueue.deleteMin())
          every(ops) must be(null)
        }
      }
    }

    "when non empty after invoking enqueue one time" - {
      "must not be empty and have size 1 after an enqueue is invoked" in {
        new Queue[Integer] {
          val queue = emptyQueue
          queue enqueue 1
          queue must (not be empty and have size 1)
        }
      }

      "must be empty after respectively calling the enqueue and deleteMin methods" in {
        new Queue[Integer] {
          val queue = emptyQueue
          queue enqueue 1
          queue.deleteMin()
          queue mustBe empty
        }
      }
    }

    "when enqueued with 9, 15, 4, and 8" - {
      """must have size of 4, be equal to 4 when peekMin is invoked,
        |have size of 3 after invoking deleteMin once,
        |and peekMin must be evaluated to 8""".stripMargin in {
        new Queue[Integer] {
          val queue = emptyQueue
          queue enqueue 9 ; queue enqueue 15; queue enqueue 4; queue enqueue 8

          queue must have size 4
          queue.peekMin mustEqual 4
          queue.deleteMin()
          queue must have size 3
          queue.peekMin mustEqual 8
        }
      }
    }
  }
}
