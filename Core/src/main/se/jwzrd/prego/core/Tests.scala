package se.jwzrd.prego.core

import java.lang.String
import server.http.{HttpServer, Expression}
import util.matching.Regex
import java.net.InetSocketAddress

/**
 * @author Patrik Andersson <pandersson@gmail.com>
 */
object Tests {
  import server.http._
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
          <p><a href="/foo/1/hello">Foo 1</a></p>
          <p><a href="/foo/2/world">Foo 2</a></p>
          <p><a href="/foo//world">Foo (empty)</a></p>
        </body>
      </html>

    GET ("/foo/[:bar]/:baz") ==>
      <html>
        <head>
          <title>Shopping cart</title>
        </head>
        <body>
          <h1>This is the foo section!</h1>
          <p>{request.path}</p>
          <p>bar: {'bar <=> "7"}</p>
          <p>baz: {'baz <=}</p>
        </body>
      </html>

    GET ("/bar") ==>
      <html>
        <head>
          <title>Shopping cart</title>
        </head>
        <body>
          <h1>This is the foo section!</h1>
          <form action="/bar-action" method="POST">
            <input type="text" name="foo" />
            <input type="submit" value="save" />
          </form>
        </body>
      </html>

    // This does not work. It appears not to send anything.
    POST ("/bar-action") ==>
      <html>
        <head>
          <title>Shopping cart</title>
        </head>
        <body>
          <h1>This is the foo section!</h1>
          <p>{request.path}</p>
          <p>{request.headers}</p>
          <p>{parameters}</p>
        </body>
      </html>
  }

  def main(args: Array[String]) =
    HttpServer (new InetSocketAddress(8181), Module(ShoppingCart, NotFound)).run
}