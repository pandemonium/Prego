package se.jwzrd.prego.core.web

import se.jwzrd.prego.core._
import database.Model
import database.Model.Log
import server._
import http._
import Application._
import java.util.Date
import java.net.InetSocketAddress
import org.squeryl.PrimitiveTypeMode._
import xml.{Node, NodeSeq}
import runtime.{Int}
import java.io.IOException

/**
 * @author Patrik Andersson <pandersson@gmail.com>
 */
object LogScreen extends Application {
  import Model._

  def renderScreen(title: String)(content: NodeSeq): Response = {
    <html>
      <head>
        <title>{title}</title>
      </head>
      <body>
        <h1>{title}</h1>
        {content}
      </body>
    </html>
  }

  def findAllLogs = transaction {
    from (Model.logs) { l => select (l) } toSeq
  }

  def renderLogList =
    <ul>{findAllLogs map renderLog}</ul>

  GET ("/log") ==> renderScreen ("Log list") {
    <div id="logList">
      {renderLogList}
    </div>
  }

  GET ("/log/edit/[:id]") ==> renderScreen ("Log editor") {
    // pass optional nextUrl to this
    val id = 'id <=> "0" toLong
    val log = transaction (logs lookup id getOrElse Log())

    <div id="editor">
      <form action={"/log/edit/" + id + "/save"} method="POST">
        <p>{"id: " + id}</p>
        Name: <input type="text" name="name" value={log name}/>
        <input type="submit" value="Save"/>
      </form>
    </div>
  }

  POST ("/log/edit/[:id]/save") ==> transaction {
    ('id <=> "0" toLong match {
      case 0 => Right (logs insertOrUpdate new Log ('name <=, new Date ()))
      case id => logs lookup id map { l =>
        l.name = 'name <=

        logs insertOrUpdate l
      } toRight StatusReply (404, "No 'log' by id: " + id)
    }) fold (identity, l => Redirect("/log/edit/" + l.id))
  }

  def renderLog(log: Log): NodeSeq =
    <li>{log name} ({log date})</li>

  println(Model)

  def main(args: Array[String]) =
    HttpServer (new InetSocketAddress(8181),
               Module (LogScreen, NotFound)).run
}