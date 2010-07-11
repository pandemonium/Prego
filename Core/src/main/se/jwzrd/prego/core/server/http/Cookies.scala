package se.jwzrd.prego.core.server.http

import java.util.Date
import se.jwzrd.prego.core.server.http.Application.{Route, ExecuteApplication}

/**
 * @author Patrik Andersson <pandersson@gmail.com>
 */

object Cookie {
  def parseMultiple(cookieHeader: String): Seq[Cookie] = cookieHeader split ";" map (_.split("=")) map {
    case Array(name, value) => Cookie(name, value)
  }

  def apply(name: String, value: String, expires: Date = null): Cookie =
    new Cookie (name, value, expires)
}

class Cookie (val name: String, val value: String, val expires: Date = null) {
  override def toString = "Cookie " + name + "=" + value

  def textRendering =
    name + "=" + value +
    (if (expires != null)
      "; expires=" + expires.toGMTString
    else
      "") +
    "; path=/; domain="
}

trait CookieHandling extends ExecuteApplication { ApplicationLike =>
  override abstract def execute(route: Route,
                                request: Request,
                                invocation: Invocation): Response = {
    // Pick up all Cookie-headers from Request.headers
    // add each to Invocation by parsing them using Cookie
    val newInvocation = request.headers filterKeys ("Cookie" ==) map {
      case (k, v) => Cookie parseMultiple(v)
    } flatten match {
      case xs if !(xs isEmpty) => invocation copy (cookies = invocation.cookies ++ xs)
      case _ => invocation
    }

    super.execute(route, request, newInvocation)
  }
}