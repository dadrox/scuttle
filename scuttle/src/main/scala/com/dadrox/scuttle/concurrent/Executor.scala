package com.dadrox.scuttle.concurrent

import java.util.concurrent.ScheduledThreadPoolExecutor
import java.util.concurrent.ThreadFactory
import java.util.concurrent.RejectedExecutionHandler
import java.util.concurrent.TimeUnit
import java.util.concurrent.Callable
import java.util.Collection
import java.util.concurrent.Future
import java.util.concurrent.ExecutorService
import com.dadrox.scuttle.time._
import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.ThreadPoolExecutor.AbortPolicy

object Executor {
    val count = new AtomicLong(1)
    def next = count.getAndIncrement()
}

class Executor(
    corePoolSize: Int = 2 * Runtime.getRuntime.availableProcessors,
    threadFactory: ThreadFactory = new NamedThreadFactory("Executor", false),
    rejectedExecutionHandler: RejectedExecutionHandler = new AbortPolicy())
        extends ExecutorService {
    import Executor._

    val underlying = new ScheduledThreadPoolExecutor(corePoolSize, threadFactory, rejectedExecutionHandler)

    def awaitTermination(timeout: Long, unit: TimeUnit): Boolean = underlying.awaitTermination(timeout, unit)

    def invokeAll[T](tasks: Collection[_ <: Callable[T]]): java.util.List[Future[T]] = underlying.invokeAll(tasks)

    def invokeAll[T](tasks: Collection[_ <: Callable[T]], timeout: Long, unit: TimeUnit): java.util.List[Future[T]] = underlying.invokeAll(tasks, timeout, unit)

    def invokeAny[T](tasks: Collection[_ <: Callable[T]]): T = underlying.invokeAny(tasks)

    def invokeAny[T](tasks: Collection[_ <: Callable[T]], timeout: Long, unit: TimeUnit): T = underlying.invokeAny(tasks, timeout, unit)

    def isShutdown: Boolean = underlying.isShutdown()

    def isTerminated: Boolean = underlying.isTerminated()

    def shutdown(): Unit = { underlying.shutdown() }

    def shutdownNow(): java.util.List[Runnable] = underlying.shutdownNow()

    override def submit(task: Runnable): Future[_] = underlying.submit(task)

    override def submit[T](task: Callable[T]): Future[T] = underlying.submit(task)

    override def submit[T](task: Runnable, result: T): Future[T] = underlying.submit(task, result)

    override def execute(task: Runnable): Unit = {

        val start = Time.now
        def time = Time.now.since(start)
        val id = next
//        println(s"executing task [$id] on ${Thread.currentThread()}")

        val result = underlying.execute(task)

//        println(s"done executing task [$id] in $time")

        result
    }
}