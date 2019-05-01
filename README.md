### Task 2 - research : Solution

The MultiQueue in my solution is based on an **AtomicReferenceArray** of Java Booleans. Each element of the array refers to an underlying priority queue 
and indicates if the queue is locked (true) or not. To insert an element into the MultiQueue, we use **compareAndSet** to check if a random priority queue
is locked or not. In case the random queue is not locked, we lock it and then perform our enqueue operation. This way, we assure that our MultiQueue supports
a Multi-threaded environment.

To check the size of our MultiQueue, we use an **AtomicInteger** that gets incremented and decremented respectively after each insertion and deleteMin call. Note
that the deleteMin function is based on [https://arxiv.org/pdf/1411.1209.pdf]. Moreover, the priority queue implementation has been simplified by using the head
method on the underlying queue. The rational behind this change is that the worse case time complexity of these method is O(1). Also, I am not such a fan of null; so,
I encourage using Either, Option or Try instead.
 