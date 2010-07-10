package se.jwzrd.prego.core

import java.lang.String
import java.net.{InetSocketAddress}

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
          <form enctype="utf-8" action="/bar-action" method="POST">
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
          <p>{'foo <=}</p>
        </body>
      </html>
  }

  def main(args: Array[String]) = {
    val mappings = Map(
      "/prego" -> """C:\Users\Maria\IdeaProjects\Prego\exportToHTML\se\jwzrd\prego\core\server""",
      "/skp" -> """C:\Users\Maria\Documents\patrik\test"""
    )
    val fileServer = FileServer("/fileserve", mappings)
    val composition = Module(fileServer, ShoppingCart, NotFound)
    HttpServer (new InetSocketAddress(8181), composition).run
  }
}