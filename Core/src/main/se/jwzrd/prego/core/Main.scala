package se.jwzrd.prego.core

import database.Model
import java.net.InetSocketAddress
import server.Deployment
import server.http.{FileServer, Application, HttpServer}
import tools.nsc.io.Path
import web.{DesktopModule, SessionModule, LoginModule, LogModule}

/**
 * @author Patrik Andersson <pandersson@gmail.com>
 */
object Main extends Deployment {
  import Application._

  println (Model)

  val mappings = Map("/file" -> "/Users/pa/Documents/Projects/Prego/root")

  val port = 8181
  val configuration = Module (DesktopModule,
                              SessionModule,
                              LoginModule,
                              LogModule,
                              FileServer("/serve", mappings),
                              NotFound)

  def main(args: Array[String]) = run
}