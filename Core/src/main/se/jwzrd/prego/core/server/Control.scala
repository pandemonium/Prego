package se.jwzrd.prego.core.server

import annotation.tailrec
import java.util.concurrent.Executor

/**
 * @author Patrik Andersson <pandersson@gmail.com>
 */
trait Control {
  implicit def byNameIsRunnable(body: => Unit): Runnable = new Runnable {
    def run = body
  }

  def run: Unit

  def stop: Unit

  def repeatedly(body: => Unit) = {
    @tailrec
    def eternally(body: => Unit): Unit = {
      body; eternally(body)
    }

    eternally(body)
  }

  def concurrently(body: => Unit)(implicit executor: Executor) =
    executor execute body
}