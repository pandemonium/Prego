package se.jwzrd.prego.core.web

import se.jwzrd.prego.core._
import database.{AccountOperations, SessionOperations, LogOperations, Model}
import util._
import server._
import http._
import Application._
import org.squeryl.PrimitiveTypeMode._
import xml.{Node, NodeSeq}
import java.util.Date

/**
 * Is this mix-in style optimal? Is everything going to compose itself
 * with everything else?
 * @author Patrik Andersson <pandersson@gmail.com>
 */
object SessionModule extends Application
                        with LogOperations
                        with SessionOperations
                        with SessionAccountAccess
                        with AccountOperations {
  import Model._

  val NotFound = StatusReply(404, "No such session")

  /*
   * This incenssant toLong:ing is extremely ugly and ought to be easily avoidable.
   * Invent something to deal with it.
   */

  GET ("/session/:sessionId") ==> {
    findSessionById (('sessionId <=) toLong) fold (renderSession, NotFound)
  }

  GET ("/session/[:sessionId]/edit") ==> {
    applyEditor (renderSessionEditor)
  }

  GET ("/session/[:sessionId]/edit/fragment") ==> {
    applyEditor (renderSessionEditorFragment)
  }

  def applyEditor(editor: Session => Response): Response =
    editor (findOrCreateNew (('sessionId <=?) map (_ toLong)))

  // Is it really necessary to differentiate a NotFound session from simply creating
  // a new one should id be None?
  def findOrCreateNew(id: Option[Long]): Session = transaction {
    id flatMap findSessionById getOrElse createSession
  }

  /**
   * Create a new session initializing it to the active (possibly also new) log;
   * setting microCycle the same as the last one (or 1 if it does not exist)
   * and numberInCycle to one more than the last one (or 1)
   */
  def createSession: Session = {
    createSession (findOrCreateLog)
  }

  def createSession(log: Log): Session = transaction {
    val (currentMicroCycle, nextNumberInCurrentCycle) =
      findLastSessionByLog (log) map { s =>
        (s.microCycle, s.numberInCycle + 1)
      } getOrElse (1, 1)

    sessions insertOrUpdate Session(log, new Date, currentMicroCycle, nextNumberInCurrentCycle)
  }

  // This pattern is very common
  def findOrCreateLog: Log = findLastLog (loggedInAccount) getOrElse createLog (loggedInAccount)

/*
  POST ("/log/:logId/session/[:sessionId]") ==> {

  }
*/

/*
  POST ("/log/edit/[:id]") ==> transaction {
    ('id <=> "0" toLong match {
      case 0 => Right (logs insertOrUpdate Log (loggedInAccount, 'name <=, new Date ()))
      case id => logs lookup id map { l =>
        l.name = 'name <=

        logs insertOrUpdate l
      } toRight StatusReply (404, "No 'log' by id: " + id)
    }) fold (identity, l => Redirect("/log/edit/" + l.id))
  }
*/


//  GET ("/log/:logId/session/") ==> renderSessionList (log)

  def renderSessionList(sessions: Seq[Session]): Response = {
    NotFound
  }

  def getLogName(session: Session): String = transaction {
    session.log.single.name
  }

  def renderSessionEditorFragment(session: Session) =
    <form action={"/session/" + session.id + "/edit"} method="POST">
      <table>
        <tr>
          <td>Log</td><td>{getLogName (session)}</td>
        </tr>
        <tr>
          <td>Date</td><td><input type="text" name="date" value={session.date.toString}/></td>
        </tr>
        <tr>
          <td>Index in cycle</td><td><input type="text" name="numberInCycle" value={session.numberInCycle.toString}/></td>
        </tr>
        <tr>
          <td>Micro cycle</td><td><input type="text" name="microCycle" value={session.microCycle.toString}/></td>
        </tr>
      </table>
    </form>

  // Invent URL / linking tools to do: edit(session) -> GET/POST /session/<id>/edit ?
  def renderSessionEditor(session: Session) = renderScreen("Session editor") {
    renderSessionEditorFragment (session)
  }

  def renderSession(session: Session) = renderScreen("Session") {
    <table>
      <tr>
        <td>Log</td><td>{getLogName (session)}</td>
      </tr>
      <tr>
        <td>Index in cycle</td><td>{session date}</td>
      </tr>
      <tr>
        <td>Micro cycle</td><td>{session microCycle}</td>
      </tr>
      <tr>
        <td>Number in cycle</td><td>{session numberInCycle}</td>
      </tr>
    </table>
  }

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
}