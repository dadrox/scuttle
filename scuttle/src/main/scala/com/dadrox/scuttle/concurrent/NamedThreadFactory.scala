package com.dadrox.scuttle.concurrent

import java.util.concurrent.ThreadFactory
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.BlockContext
import scala.concurrent.CanAwait
import scala.concurrent.forkjoin.ForkJoinPool
import scala.concurrent.forkjoin.ForkJoinPool.ForkJoinWorkerThreadFactory
import scala.concurrent.forkjoin.ForkJoinWorkerThread

object NamedThreadFactory {
    def forkJoin(name: String, daemonThreads_? : Boolean = true): ForkJoinWorkerThreadFactory = new NamedForkJoinThreadFactory(name, daemonThreads_?)
    def standard(name: String, daemonThreads_? : Boolean = true): ThreadFactory = new NamedThreadFactory(name, daemonThreads_?)
}

class NamedThreadFactory(name: String, daemonThreads_? : Boolean = false) extends ThreadFactory {
    val currentNumber = new AtomicInteger(1)
    val group = new ThreadGroup(Thread.currentThread().getThreadGroup(), name)

    def newThread(r: Runnable): Thread = {
        val numberedName = s"$name-${currentNumber.getAndIncrement()}"
        val thread = new Thread(group, r, numberedName)
        thread.setName(numberedName)
        thread.setDaemon(daemonThreads_?)
        thread
    }
}

class NamedForkJoinWorkerThread(
    pool: ForkJoinPool,
    name: String = "ForkJoinWorkerThread",
    daemon_? : Boolean = false)
        extends ForkJoinWorkerThread(pool) {
    this.setName(name)
    this.setDaemon(daemon_?)
}

class NamedForkJoinThreadFactory(
    name: String,
    daemonThreads_? : Boolean = true)
        extends ForkJoinWorkerThreadFactory {

    val currentNumber = new AtomicInteger(1)

    //    def newThread(pool: ForkJoinPool): ForkJoinWorkerThread = {
    //        val numberedName = s"$name-${currentNumber.getAndIncrement()}"
    //        new NamedForkJoinWorkerThread(pool, numberedName, daemonThreads_?)
    //    }

    def wire[T <: Thread](thread: T): T = {
        thread.setDaemon(daemonThreads_?)
        //        thread.setUncaughtExceptionHandler(uncaughtExceptionHandler)
        thread
    }

    def newThread(runnable: Runnable): Thread = wire(new Thread(runnable))

    def newThread(fjp: ForkJoinPool): ForkJoinWorkerThread = {
        val numberedName = s"$name-${currentNumber.getAndIncrement()}"
        wire(new NamedForkJoinWorkerThread(fjp, numberedName, daemonThreads_?) with BlockContext {
            override def blockOn[T](thunk: => T)(implicit permission: CanAwait): T = {
                var result: T = null.asInstanceOf[T]
                ForkJoinPool.managedBlock(new ForkJoinPool.ManagedBlocker {
                    @volatile var isdone = false
                    override def block(): Boolean = {
                        result = try thunk finally { isdone = true }
                        true
                    }
                    override def isReleasable = isdone
                })
                result
            }
        })
    }

}