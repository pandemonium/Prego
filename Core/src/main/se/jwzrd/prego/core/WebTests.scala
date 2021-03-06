package se.jwzrd.prego.core

import java.lang.String
import java.net.{InetSocketAddress}
import java.util.Date
import web.LogModule

/**
 * @author Patrik Andersson <pandersson@gmail.com>
 */
object WebTests {
  import server.http._
  import server.http.Application._

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

    GET ("/foo/[:bar]/:baz") ==> {
      <html>
        <head>
          <title>Shopping cart</title>
        </head>
        <body>
          <h1>This is the foo section!</h1>
          <p>
            {request.path}
          </p>
          <p>bar:
            {'bar <=> "7"}
          </p>
          <p>baz:
            {'baz <=}
          </p>
        </body>
      </html> set Cookie("1", "2", new Date())
    }

    def view =
      <html>
        <head>
          <title>Shopping cart</title>
        </head>
        <body>
          <h1>Shop shite here!</h1>
          <p>{request.path}</p>
          <p>Cookies: {cookies.mkString(",")}</p>
          <p>Headers: <ul>{request.headers map { case (k, v) => <li>{k} = {v}</li>  }}</ul></p>
          <form action="/set-cookie" method="POST">
            <input type="text" name="cookieName" />
            <input type="submit" value="save" />
          </form>
        </body>
      </html>

    GET ("/cookies") ==> view

    POST ("/set-cookie") ==> {
      view set Cookie('cookieName <=, "hello", new Date(System.currentTimeMillis + 2000000))
    }

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
          <p>{'foo <=}</p>
        </body>
      </html>

    def sessionView =
      <html>
        <head>
          <title>Shopping cart</title>
        </head>
        <body>
          <h1>Session contains</h1>
          <p>{request.path}</p>
          <p>Headers: <ul>{request.headers map { case (k, v) => <li>{k} = {v}</li>  }}</ul></p>
          <p>Session: <ul>{session map { case (k, v) => <li>{k} = {v}</li>  }}</ul></p>
          <form action="/session-set" method="POST">
            <input type="text" name="name" />
            <input type="text" name="value" />
            <input type="submit" value="save" />
          </form>
        </body>
      </html>

    GET ("/session") ==> sessionView

    POST ("/session-set") ==> {
      trait StringKeyLike extends Key with Proxy {
        type Value = AnyRef
        val value: String
        override def self = value
      }

      case class StringKey(value: String) extends StringKeyLike

      session(StringKey ('name <=)) = 'value <=

      sessionView
    }
  }

  def main(args: Array[String]) = {

    val mappings = Map(
      "/prego" -> """/Users/pa/Documents/Projects/Prego/exportToHTML/se/jwzrd/prego/core"""
    )

    val fileServer = FileServer("/fileserve", mappings)
    val composition = Module(LogModule, NotFound)
    HttpServer (new InetSocketAddress(8181), composition).run
  }
}