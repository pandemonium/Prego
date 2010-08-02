package se.jwzrd.prego.core.web

import se.jwzrd.prego.core._
import database.Model.Account
import database.{LogOperations, Model}
import server._
import http._
import util._
import Application._
import org.squeryl.PrimitiveTypeMode._
import xml.{Node, NodeSeq}

/**
 * @author Patrik Andersson <pandersson@gmail.com>
 */
object DesktopModule extends Application
                        with SessionAccountAccess {
  GET ("/") ==> renderDesktop ("Prego/0.1") {
    loggedInAccountOption fold (renderDefaultDesktop,
                                LoginModule loginForm "/")
  }

  def renderDefaultDesktop(account: Account): NodeSeq = {
    <div id="functionGroupTabs">
      <ul>
        <li><a href="/session//edit/fragment">New session</a></li>
        <li><a href="#functionGroupTabs-2">Daily log</a></li>
        <li><a href="#functionGroupTabs-3">Weekly summary</a></li>
        <li><a href="#functionGroupTabs-4">Exercise statistics</a></li>
      </ul>
      <div id="functionGroupTabs-2">Phasellus mattis tincidunt nibh. Cras orci urna, blandit id, pretium vel, aliquet ornare, felis. Maecenas scelerisque sem non nisl. Fusce sed lorem in enim dictum bibendum.</div>
      <div id="functionGroupTabs-3">Nam dui erat, auctor a, dignissim quis, sollicitudin eu, felis. Pellentesque nisi urna, interdum eget, sagittis et, consequat vestibulum, lacus. Mauris porttitor ullamcorper augue.</div>
      <div id="functionGroupTabs-4">Nam dui erat, auctor a, dignissim quis, sollicitudin eu, felis. Pellentesque nisi urna, interdum eget, sagittis et, consequat vestibulum, lacus. Mauris porttitor ullamcorper augue.</div>
    </div>
  }

  def inlineCss: String =
    """
      body{ font: 62.5% sans-serif; margin: 50px;}
      #button {padding: .4em 1em .4em 20px;text-decoration: none;position: relative;}
      #button span.ui-icon {margin: 0 5px 0 0;position: absolute;left: .2em;top: 50%;margin-top: -8px;}
    """

  def inlineJavascript: String =
    """
      $(function() {
        $('#functionGroupTabs').tabs();
      });

    """

  // Find a decent jquery "reset" css.
  def renderDesktop(title: String)(body: => NodeSeq): NodeSeq = {
    <html>
      <head>
        <title>{title}</title>
        <link type="text/css" href="/serve/file/jquery/jquery-ui-1.8.2.custom.css" rel="stylesheet" />
        <script type="text/javascript" src="/serve/file/jquery/jquery-1.4.2.min.js"></script>
        <script type="text/javascript" src="/serve/file/jquery/jquery-ui-1.8.2.custom.min.js"></script>
        <style type="text/css">{inlineCss}</style>
        <script type="text/javascript">{inlineJavascript}</script>
      </head>
      <body>
        <h1>{title}</h1>
        {body}
      </body>
    </html>
  }
}