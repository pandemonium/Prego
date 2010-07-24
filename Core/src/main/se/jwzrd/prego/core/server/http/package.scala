package se.jwzrd.prego.core.server

import http.Application.{HttpMethod, RouteSource, IntrinsicValues}
import http.{Application, CookieDecoration, Response, Request}

/**
 * @author Patrik Andersson <pandersson@gmail.com>
 */
package object http {
  type RequestProcessor = Request => Response

  implicit def responseLikeCanHaveCookies[A <% Response](response: A): CookieDecoration[A] =
    new CookieDecoration (response)

  implicit def symbolGrantsParameterAccess(s: Symbol) = new ParameterAccess {
    val symbol: Symbol = s
  }

  implicit def httpMethodCanBeRouteSource(method: HttpMethod): RouteSource =
    new RouteSource (method)

  trait ParameterAccess {
    val symbol: Symbol

    // This really isn't very good
    def <=> (default: String)(implicit iv: IntrinsicValues) =
      iv.parameters getOrElse(symbol name, default)

    // This really isn't very good
    def <= (implicit iv: IntrinsicValues): String =
      iv.parameters(symbol name)

    def <=? (implicit iv: IntrinsicValues): Option[String] =
      iv.parameters get(symbol name)
  }

  import Application._
  val GET = HttpMethod.GET
  val HEAD = HttpMethod.HEAD
  val PUT = HttpMethod.PUT
  val POST = HttpMethod.POST
  val OPTION = HttpMethod.OPTION
  val DELETE = HttpMethod.DELETE
}