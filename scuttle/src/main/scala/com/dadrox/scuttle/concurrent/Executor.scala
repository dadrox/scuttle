package com.dadrox.scuttle.concurrent

import com.dadrox.scuttle.time._
import java.util.Collection
import java.util.concurrent.Callable
import java.util.concurrent.ExecutorService
import java.util.concurrent.Future
import java.util.concurrent.RejectedExecutionHandler
import java.util.concurrent.SynchronousQueue
import java.util.concurrent.ThreadFactory
import java.util.concurrent.ThreadPoolExecutor
import java.util.concurrent.ThreadPoolExecutor.AbortPolicy
import java.util.concurrent.TimeUnit
import scala.concurrent.forkjoin.ForkJoinPool
import scala.concurrent.forkjoin.ForkJoinPool.ForkJoinWorkerThreadFactory

object ForkJoinExecutor {
    lazy val DefaultExceptionHandler = NopExceptionHandler

    lazy val NopExceptionHandler = new Thread.UncaughtExceptionHandler {
        def uncaughtException(thread: Thread, cause: Throwable): Unit = reportingExceptionHandler(_ => ())
    }

    lazy val ConsoleExceptionHandler = new Thread.UncaughtExceptionHandler {
        def uncaughtException(thread: Thread, cause: Throwable): Unit = reportingExceptionHandler(t => t.printStackTrace())
    }

    def reportingExceptionHandler(reporter: Throwable => Unit) = new Thread.UncaughtExceptionHandler {
        def uncaughtException(thread: Thread, cause: Throwable): Unit = reporter(cause)
    }
}

object Executor {
    def forkJoin(
        corePoolSize: Int = 2 * Runtime.getRuntime.availableProcessors,
        threadFactory: ForkJoinWorkerThreadFactory = new NamedForkJoinThreadFactory("ForkJoinExecutor", false),
        handler: Thread.UncaughtExceptionHandler = ForkJoinExecutor.DefaultExceptionHandler) =
        new ForkJoinExecutor(corePoolSize, threadFactory, handler)

    def executor(
        corePoolSize: Int = 2 * Runtime.getRuntime.availableProcessors,
        maxPoolSize: Int = Int.MaxValue,
        threadFactory: ThreadFactory = new NamedThreadFactory("Executor", false),
        rejectedExecutionHandler: RejectedExecutionHandler = new AbortPolicy()) =
        new Executor(corePoolSize, maxPoolSize, threadFactory, rejectedExecutionHandler)
}

trait ExecutorServiceDelegate extends ExecutorService {
    def underlying: ExecutorService

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
    override def execute(task: Runnable): Unit = underlying.execute(task)
}

class ForkJoinExecutor(
    corePoolSize: Int = 2 * Runtime.getRuntime.availableProcessors,
    threadFactory: ForkJoinWorkerThreadFactory = new NamedForkJoinThreadFactory("ForkJoinExecutor", false),
    handler: Thread.UncaughtExceptionHandler = ForkJoinExecutor.DefaultExceptionHandler)
        extends ExecutorServiceDelegate {

    override val underlying = new ForkJoinPool(corePoolSize, threadFactory, handler, false)
}

class Executor(
    corePoolSize: Int = 2 * Runtime.getRuntime.availableProcessors,
    maxPoolSize: Int = Int.MaxValue,
    threadFactory: ThreadFactory = new NamedThreadFactory("Executor", false),
    rejectedExecutionHandler: RejectedExecutionHandler = new AbortPolicy())
        extends ExecutorServiceDelegate {

    override val underlying = new ThreadPoolExecutor(corePoolSize, maxPoolSize, 60L, TimeUnit.SECONDS, new SynchronousQueue[Runnable](), threadFactory, rejectedExecutionHandler)
}