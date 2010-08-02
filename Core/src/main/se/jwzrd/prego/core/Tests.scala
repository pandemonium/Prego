package se.jwzrd.prego.core

import scalaz._
import Scalaz._

/**
 * @author Patrik Andersson <pandersson@gmail.com>
 */
object Tests {
  case class Foo(bar: Int, baz: Int)

  def main(args: Array[String]) {
    val m = Map(1 -> 2, 2 -> 3)

    println(m(3))
  }
}