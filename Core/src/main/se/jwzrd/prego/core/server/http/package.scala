package se.jwzrd.prego.core.server

import http.Application.IntrinsicValues
import http.{Response, Request}

/**
 * @author Patrik Andersson <pandersson@gmail.com>
 */
package object http {
  type RequestProcessor = Request => Response

  implicit def symbolGrantsParameterAccess(s: Symbol) = new ParameterAccess {
    val symbol: Symbol = s
  }

  trait ParameterAccess {
    val symbol: Symbol

    def <=> (default: String)(implicit iv: IntrinsicValues) =
      iv.parameters getOrElse(symbol name, default)

    def <= (implicit iv: IntrinsicValues) =
      iv.parameters(symbol name)
  }
}