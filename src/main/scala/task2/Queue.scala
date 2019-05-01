package task2

class Queue[A >: Null] private (
  underlying: scala.collection.mutable.PriorityQueue[A]
) {
  def isEmpty: Boolean = underlying.isEmpty

  def deleteMin(): A = if (isEmpty) null else underlying.dequeue()

  def peekMin: A = if(isEmpty) null else underlying.head

  def enqueue(queued: A): Unit = underlying.enqueue(queued)

  def size: Int = underlying.size
}

object Queue {

  def empty[A >: Null](implicit ord: Ordering[A]): Queue[A] = {
    new Queue[A](new scala.collection.mutable.PriorityQueue()(ord.reverse))
  }
}
