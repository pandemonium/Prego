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
object LoginModule extends Application with AccountOperations with SessionAccountAccess {
  GET ("/login") ==> loginScreen

  POST ("/login") ==> {
    val credentials: Option[(String, String)] = ('username <=?) zip ('password <=?)
    val authentication = authenticate _ tupled

    credentials flatMap authentication fold (login, loginScreen)
  }

  def login(account: Account): Response = {
    loggedInAccount = account

    Redirect ('redirectBack <=> "/")
  }

  // This isn't very useful at the moment
  def loginScreen: Response =
    <html>
      <head>
        <title>Login</title>
      </head>
      <body>{loginForm ("/")}</body>
    </html>

  // Overlay labels for username and password
  def loginForm(redirectBack: String) =
    <div id="loginForm">
      <div>Login</div>
      <form action="/login" method="POST">
        <input type="hidden" name="redirectBack" value={redirectBack}/>
        <input type="text" name="username" value={'username <=> ""}/>
        <input type="password" name="password"/>
        <input type="submit" value="Login"/>
      </form>
    </div>
}