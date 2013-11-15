package com.dadrox.scuttle.time

import com.dadrox.scuttle.result.Future
import java.util.concurrent.ThreadFactory
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.Executors
import scala.concurrent.Promise
import com.dadrox.scuttle.result._
import scala.util.control.NonFatal

trait TimerTask {
    def cancel()
}

trait Timer {
    def repeat(period: Duration)(fn: => Unit): TimerTask = repeat(Time.now, period)(fn)
    def repeat(start: Time, period: Duration)(fn: => Unit): TimerTask

    def doIn[A](duration: Duration)(fn: => A): Future[A] = doAt(Time.now + duration)(fn)
    def doAt[A](time: Time)(fn: => A): Future[A]

    def stop()
}

case class FailedScheduledTask(cause: Option[Throwable]) extends Failure.Detail {
    val reason = new Failure.Reason {
        val name = "FailedScheduledTask"
    }
    val message = "Unexpected exception on scheduled task"
        override val toString = s"FailedScheduledTask($message, $cause)"
}

class PooledTimer(threads: Int = 2, threadFactory: ThreadFactory) extends Timer {
    def this(name: String, threads: Int = 2, daemonThreads: Boolean = false) = this(threads, new NamedThreadFactory(name, daemonThreads))

    val executor = Executors.newScheduledThreadPool(threads, threadFactory)

    def repeat(start: Time, period: Duration)(fn: => Unit): TimerTask = {
        val r = new Runnable { def run = fn }
        val javaFuture = executor.scheduleAtFixedRate(r, Time.now.until(start).inMilliseconds(), period.inMilliseconds(), java.util.concurrent.TimeUnit.MILLISECONDS)
        new TimerTask {
            def cancel() = {
                javaFuture.cancel(true)
            }
        }
    }

    def doAt[A](when: Time)(fn: => A): Future[A] = {
        val p = Promise[Result[A]]()

        val r = new Runnable {
            def run = {
                p.completeWith(Promise.successful {
                    try Success(fn)
                    catch {
                        case NonFatal(e) => Failure(FailedScheduledTask(Some(e)))
                    }
                }.future)
            }
        }
        val javaFuture = executor.schedule(r, Time.now.until(when).inMilliseconds(), java.util.concurrent.TimeUnit.MILLISECONDS)
        val task = new TimerTask {
            def cancel() = {
                javaFuture.cancel(true)
            }
        }
        ConcreteFuture(p.future)
    }

    def stop() {
        executor.shutdown()
    }
}

class NamedThreadFactory(name: String, daemonThreads: Boolean = false) extends ThreadFactory {
    val currentNumber = new AtomicInteger(1)
    val group = new ThreadGroup(Thread.currentThread().getThreadGroup(), name)

    def newThread(r: Runnable): Thread = {
        val thread = new Thread(group, r, s"$name-${currentNumber.getAndIncrement()}")
        thread.setName(name)
        thread.setDaemon(daemonThreads)
        thread
    }
}