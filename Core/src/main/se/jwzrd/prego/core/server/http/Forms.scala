package se.jwzrd.prego.core.server.http

import se.jwzrd.prego.core.server.http.Application.{Route, ApplicationExecution}

/**
 * @author Patrik Andersson <pandersson@gmail.com>
 */

trait WwwFormHandling extends ApplicationExecution { ApplicationLike =>
  // check Content-Type header to know to engage
  // read Content-Length to know exactly how much to read
  // read and parse
  // put read data into invocation.parameters
  override abstract def execute(route: Route,
                                request: Request,
                                invocation: Invocation): Response = {
    request.headers get ("Content-Type") match {
      case Some("application/x-www-form-urlencoded") =>
        handleWwwFormUrlEncoded(route, request, invocation)
      case _ =>
        super.execute(route, request, invocation)
    }
  }

  def handleWwwFormUrlEncoded(route: Route,
                              request: Request,
                              invocation: Invocation): Response = {
    val contentLength = request headers ("Content-Length") toInt
    val body = (request data) take (contentLength)

    super.execute (route,
                   request,
                   invocation copy (defaultParameters = parseParameters (body)))
  }

  def parseParameters(body: Iterator[Char]) =
    parsePostBody (body) toMap

  import java.net.URLDecoder.decode
  def parsePostBody(body: Iterator[Char]) = parameterPairs (body) map parseParameter map {
    case Array(k, v) => (decode(k) -> decode(v))
  }

  def parameterPairs(body: Iterator[Char]) =
    (body mkString) split "&"

  def parseParameter(pair: String) =
    pair split "="
}