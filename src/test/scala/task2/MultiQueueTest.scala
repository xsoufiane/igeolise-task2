package task2

import org.scalatest.{FreeSpec, MustMatchers}

class MultiQueueTest extends FreeSpec with MustMatchers {

    trait MultiQueue[A >: Null] {
      def emptyMultiQueue(implicit ord: Ordering[A]): task2.MultiQueue[A] = MultiQueue empty 4
    }

    "A priority multiqueue of integers" - {
      "when empty" - {
        "must be of size 0  " in {
          new MultiQueue[Integer] {
            emptyMultiQueue.size.get() must equal(0)
          }
        }

        "must be empty" in {
          new MultiQueue[Integer] {
            emptyMultiQueue mustBe empty
          }
        }

        "must be null when deleteMin is invoked" in {
          new MultiQueue[Integer] {
            emptyMultiQueue.deleteMin() must be(null)
          }
        }
      }

      "when non empty after invoking enqueue one time" - {
        "must not be empty and have size 1 after an enqueue is invoked" in {
          new MultiQueue[Integer] {
            val multiQueue = emptyMultiQueue
            multiQueue insert 1
            multiQueue.size.get() must equal(1)
          }
        }

        "must be empty after respectively calling the enqueue and deleteMin methods" in {
          new MultiQueue[Integer] {
            val multiQueue = emptyMultiQueue
            multiQueue insert 1
            multiQueue.deleteMin()
            multiQueue mustBe empty
          }
        }
      }

      "when non empty after parallel insertion of 600 elements from 1 to 600 (inclusive)" - {
        "must have size of 600 and be empty after deleting all elements in parallel " in {
          new MultiQueue[Integer] {
            val multiQueue = emptyMultiQueue
            val elements = (1 to 600).par

            elements.foreach(multiQueue.insert(_))

            multiQueue.size.get() mustEqual 600

            elements.foreach(_ => {
              multiQueue.deleteMin()

              ()
            })

            multiQueue.size.get() must equal(0)
          }
        }
      }
    }
}
