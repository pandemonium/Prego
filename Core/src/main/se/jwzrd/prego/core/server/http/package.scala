package se.jwzrd.prego.core.server

import se.jwzrd.prego.core.util._
import http.Application.{HttpMethod, RouteSource, IntrinsicValues}
import http.{Application, CookieDecoration, Response, Request}
import java.lang.String

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

  trait CanConvert[A] {
    def apply(source: String): A
  }

  implicit object LongConversion extends CanConvert[Long] {
    def apply(source: String): Long = source toLong
  }

  implicit object StringConversion extends CanConvert[String] {
    def apply(source: String): String = source
  }

  // This does not work! Weird errors about ambiguities.
/*
  trait ParameterAccess {
    val symbol: Symbol

    def <=> [A](default: A)(implicit iv: IntrinsicValues, cc: CanConvert[A]): A =
      iv.parameters get(symbol name) fold (cc apply, default)

    def <= [A](implicit iv: IntrinsicValues, cc: CanConvert[A]): A =
      cc (iv parameters(symbol name))

    def <=? [A](implicit iv: IntrinsicValues, cc: CanConvert[A]): Option[A] =
      iv.parameters get(symbol name) map cc.apply
  }
*/

  trait ParameterAccess {
    val symbol: Symbol

    def <=> (default: String)(implicit iv: IntrinsicValues): String =
      iv.parameters get(symbol name) getOrElse default
//      iv.parameters get(symbol name) fold (cc apply, default)

    def <= (implicit iv: IntrinsicValues): String =
      iv parameters(symbol name)

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