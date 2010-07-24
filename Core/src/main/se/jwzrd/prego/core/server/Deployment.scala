package se.jwzrd.prego.core.server

import se.jwzrd.prego.core._
import server.http._
import Application._
import java.net.{InetAddress, InetSocketAddress}

/**
 * @author Patrik Andersson <pandersson@gmail.com>
 */
trait Deployment {
  val port: Int
  val host: InetAddress = InetAddress getLocalHost
  val configuration: Application

  def run = HttpServer (new InetSocketAddress (host, port), configuration) run
}