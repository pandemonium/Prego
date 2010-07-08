package se.jwzrd.prego.core

import java.lang.String
import server.http.{HttpServer, Expression}
import util.matching.Regex
import java.net.InetSocketAddress

/**
 * @author Patrik Andersson <pandersson@gmail.com>
 */
object Tests {
  import server.http.Application._
  import HttpMethod._

  object ShoppingCart extends Application {
    GET ("/") ==>
      <html>
        <head>
          <title>Shopping cart</title>
        </head>
        <body>
          <h1>Shop shite here!</h1>
          <p>{request.path}</p>
          <p><a href="/foo/1">Foo 1</a></p>
          <p><a href="/foo/2">Foo 2</a></p>
        </body>
      </html>

    GET ("/foo/:bar") ==>
      <html>
        <head>
          <title>Shopping cart</title>
        </head>
        <body>
          <h1>This is the foo section!</h1>
          <p>{request.path}</p>
          <p>bar: {parameters().get("bar")}</p>
        </body>
      </html>
  }

  def main(args: Array[String]): Unit = {
    HttpServer (new InetSocketAddress(8181), Module(ShoppingCart, NotFound)).run
/*
    val e = Expression ("/foo/:bar")
    println(e.evaluate("/foo/1"))
*/

    trait Foo extends PartialFunction[Int, String] {
      def isDefinedAt(i: Int) = {
        println("isDefinedAt!")

        2 == i
      }

      def apply(i: Int): String = "2"
    }

    val p: Foo = new Foo {}
    val f = p.lift

    println(f(1))
  }
}