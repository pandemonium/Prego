package se.jwzrd.prego.core.web

import se.jwzrd.prego.core._
import database.{LogOperations, Model}
import server._
import http._
import Application._
import org.squeryl.PrimitiveTypeMode._
import xml.{Node, NodeSeq}
import java.util.Date
import java.net.InetSocketAddress

/**
 * This should also contain UI for adding sessions and simliar things
 * I think. Or should it?
 * @author Patrik Andersson <pandersson@gmail.com>
 */
object LogModule extends Application
                    with LogOperations
                    with SessionAccountAccess {
  import Model._

  def renderScreen(title: String)(content: NodeSeq): Response = {
    <html>
      <head>
        <title>{title}</title>
        <link type="text/css" href="/serve/file/jquery/jquery-ui-1.8.2.custom.css" rel="stylesheet" />
        <script type="text/javascript" src="/serve/file/jquery/jquery-1.4.2.min.js"></script>
        <script type="text/javascript" src="/serve/file/jquery/jquery-ui-1.8.2.custom.min.js"></script>
      </head>
      <body>
        <h1>{title}</h1>
        {content}
      </body>
    </html>
  }

  def renderLogList =
    <ul>{findAllLogs map renderLog}</ul>

  def renderLog(log: Log): NodeSeq =
    <li><a href={"/log/" + log.id + "/edit"}>{log name} ({log createdTime})</a></li>

  GET ("/log") ==> renderScreen ("Log list") {
    <div id="logList">
      {renderLogList}
    </div>
  }

  GET ("/log/[:id]/edit") ==> renderScreen ("Log editor") {
    // pass optional nextUrl to this
    val id = 'id <=> "0" toLong
    val log: Log = transaction (logs lookup id getOrElse Log(loggedInAccount))

    <div id="editor">
      <form action={"/log/edit/" + id} method="POST">
        <p>{"id: " + id}</p>
        Name: <input type="text" name="name" value={log name}/>
        <input type="submit" value="Save"/>
      </form>
    </div>
  }

  // Must be possible to simplify this and reuse the core logic
  POST ("/log/[:id]/edit") ==> transaction {
    ('id <=> "0" toLong match {
      case 0 => Right (logs insertOrUpdate Log (loggedInAccount, 'name <=, new Date ()))
      case id => logs lookup id map { l =>
        l.name = 'name <=

        logs insertOrUpdate l
      } toRight StatusReply (404, "No 'log' by id: " + id)
    }) fold (identity, l => Redirect("/log/edit/" + l.id))
  }
}