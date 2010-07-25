package se.jwzrd.prego.core.web

import se.jwzrd.prego.core._
import database.Model._
import database.{AccountOperations, Model}
import database._
import server._
import http._
import util._
import Application._
import org.squeryl.PrimitiveTypeMode._
import xml.{Node, NodeSeq}

/**
 * Invent a way to let Login happen "when needed" so that an interstitial login can
 * take place. (The users gets back to where he was before the session timed out.)
 *
 * Is it a good idea to break-out the "code behind" into traits that are then
 * mixed-in to 'Login' for instance? (And any other screen that may need the code.)
 *
 * @author Patrik Andersson <pandersson@gmail.com>
 */
object Login extends Application with AccountOperations {
  GET ("/login") ==> loginScreen

  POST ("/login") ==> {
    val credentials = ('username <=?) zip ('password <=?)
    val authentication = authenticate _ tupled

    credentials flatMap authentication fold (login, loginScreen)
  }

  def login(account: Account): Response = {
    session (AccountKey) = account

    // get 'next' parameter - redirect to it or to default /

    <html>
      <head>
        <title>Index</title>
      </head>
      <body>
        <h1>Prego</h1>
        <p>Account: {session (AccountKey).username}</p>
        <ul>
          <li><a href="/log">Logs</a></li>
        </ul>
      </body>
    </html>
  }

  def loginScreen: Response =
    <html>
      <head>
        <title>Login</title>
      </head>
      <body>
        <form action="/login" method="POST">
          <input type="text" name="username" value={'username <=> ""}/>
          <input type="password" name="password"/>
          <input type="submit" value="Login"/>
        </form>
      </body>
    </html>
}