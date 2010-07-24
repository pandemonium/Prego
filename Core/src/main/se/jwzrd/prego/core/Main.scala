package se.jwzrd.prego.core

import database.Model
import server.http.{Application, HttpServer}
import java.net.InetSocketAddress
import server.Deployment
import web.{Login, LogScreen}

/**
 * @author Patrik Andersson <pandersson@gmail.com>
 */
object Main extends Deployment {
  import Application._

  println (Model)

  val port = 8181
  val configuration = Module (Login, LogScreen, NotFound)

  def main(args: Array[String]) = run
}