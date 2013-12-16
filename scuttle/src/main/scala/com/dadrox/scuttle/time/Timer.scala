package com.dadrox.scuttle.time

import com.dadrox.scuttle.result._
import java.util.concurrent.ThreadFactory
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.Executors
import scala.concurrent.Promise
import scala.util.control.NonFatal
import scala.collection.mutable.ArrayBuffer

case class FailedScheduledTask(cause: Option[Throwable]) extends Failure.Detail {
    val reason = new Failure.Reason {
        val name = "FailedScheduledTask"
    }
    val message = "Unexpected exception on scheduled task"
    override val toString = s"FailedScheduledTask($message, $cause)"
}

object Timer {
    def apply(threads: Int = 2, name: String = "Timer", daemonThreads: Boolean = false) =
        new PooledTimer(threads, name, daemonThreads)

    def integrationTestTimer() = new PooledTimer(threads = 1, name = "TestTimer", daemonThreads = true)

    def fake(timeSource: TimeSource = FakeTime()) = new FakeTimer(timeSource)
}

trait TimerTask {
    def cancel()
}

trait Timer {
    def repeat(period: Duration)(fn: => Unit): TimerTask = repeat(Time.now + period, period)(fn)
    def repeat(start: Time, period: Duration)(fn: => Unit): TimerTask

    def doIn[A](duration: Duration)(fn: => A): Future[A] = doAt(Time.now + duration)(fn)
    def doAt[A](time: Time)(fn: => A): Future[A]

    def stop()
}

class PooledTimer(threads: Int, threadFactory: ThreadFactory) extends Timer {
    def this(threads: Int = 2, name: String = "Timer", daemonThreads: Boolean = false) = this(threads, new NamedThreadFactory(name, daemonThreads))

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
        executor.schedule(r, Time.now.until(when).inMilliseconds(), java.util.concurrent.TimeUnit.MILLISECONDS)
        Future(p.future)
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

class FakeTimer(timeSource: TimeSource) extends Timer {
    val tasks = ArrayBuffer[Task]()

    trait Task extends TimerTask {
        def runAt: Time
        def cancel() {}
    }

    case class OneShotTask[A](runAt: Time, fn: () => A, promise: Promise[Result[A]]) extends Task
    case class PeriodicTask(runAt: Time, period: Duration, fn: () => Unit) extends Task

    def tick() {

        println(s"tick NOW ${timeSource.now} $tasks")

        val ready = tasks.filter(_.runAt <= timeSource.now)

        println(s"READY $ready")


        ready.foreach {
            case task @ OneShotTask(_, fn, promise) =>
                tasks -= task
                promise.success(Success(fn()))
            case task @ PeriodicTask(_, period, fn) =>

                println(s"BEFORE TASKS $tasks")

                fn()
                tasks -= task += task.copy(runAt = timeSource.now + period)

                println(s"NEW TASKS $tasks")
        }
    }

    def repeat(start: Time, period: Duration)(fn: => Unit): TimerTask = {
        val task = PeriodicTask(start, period, () => fn)
        tasks += task
        task
    }

    def doAt[A](time: Time)(fn: => A): Future[A] = {
        val p = Promise[Result[A]]()
        val task = OneShotTask[A](time, () => fn, p)
        tasks += task
        Future(p.future)
    }

    def stop() {}
}