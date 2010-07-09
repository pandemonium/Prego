package se.jwzrd.prego.core.server.http

import util.DynamicVariable
import java.lang.String
import collection.immutable.Map
import java.net.URLDecoder

object Application {
  object Module {
    def apply(xs: Application*) = new Composition {
      val applications = xs
    }
  }

  // I would like to be able to add state to this (or Invocation) so that I can chain
  // "request handler":s via mixin that add information. EncodingAware for instance.
  case class RequestContext(val request: Request, val invocation: Invocation)

  trait IntrinsicValues {
    def request: Request
    def parameters: Map[String, String]
  }

  trait Intrinsics extends IntrinsicValues {
    val state = new DynamicVariable[Option[RequestContext]] (None)

    def request: Request =
      state.value map (_ request) orNull

    implicit def parameters: Map[String, String] =
      state.value map (_.invocation.parameters()) orNull

    def using[A](rc: RequestContext)(thunk: => A): A =
      state.withValue (Some (rc)) (thunk)
  }

  trait Routing {
    var routes = Seq[Route] ()

    implicit val add: Route => Unit = routes :+= _
  }

  case class Route(method: HttpMethod,
                   rule: Expression,
                   action: () => Response)

  trait Origin {
    def routeTo(handler: => Response): Route

    def ==> (handler: => Response)(implicit add: Route => Unit): Unit =
      add (routeTo (handler))
  }

  implicit def httpMethodCanBeRouteSource(method: HttpMethod): RouteSource =
    new RouteSource (method)

  class RouteSource(val method: HttpMethod) {
    def apply(expressionSource: String) = new Origin {
      def routeTo(handler: => Response) =
        Route (method, Expression (expressionSource), () => handler)
    }
  }

  sealed abstract class HttpMethod (val name: String) {
    def is(name: String) = this.name == name
  }

  object HttpMethod {
    object GET extends HttpMethod ("GET")
    object HEAD extends HttpMethod ("HEAD")
    object PUT extends HttpMethod ("PUT")
    object POST extends HttpMethod ("POST")
    object OPTION extends HttpMethod ("OPTION")
    object DELETE extends HttpMethod ("DELETE")
  }

  trait Composition extends Application {
    def applications: Seq[Application]

    lazy private val lifted =
      applications map (_ lift)

    protected def applied(request: Request) =
      lifted flatMap (_ (request))

    override def isDefinedAt(request: Request) =
      !(applied (request) isEmpty)

    override def apply(request: Request) = applied (request) match {
      case x :: Nil => x
      case xs => selectOne (xs)
    }

    def selectOne(chain: Seq[Response]): Response = {
      def byStatus(s: Int) =
        chain find (_.status == s)

      // implement No Content
      byStatus (200) getOrElse {
        byStatus (500) getOrElse chain.head
      }
    }
  }

  trait WwwFormHandling extends ApplicationLike {
    // check Content-Type header to know to engage
    // read Content-Length to know exactly how much to read
    // read and parse
    // put read data into invocation.parameters
    override def execute(route: Route,
                         request: Request,
                         invocation: Invocation): Response = {
      request.headers get("Content-Type") match {
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
      Map() ++ parsePostBody (body)

    import URLDecoder.decode
    def parsePostBody(body: Iterator[Char]) = parameterPairs (body) map parseParameter map {
      case Array(k, v) => (decode(k) -> decode(v))
    }

    def parameterPairs(body: Iterator[Char]) =
      (body mkString) split "&"

    def parseParameter(pair: String) =
      pair split "="
  }

  trait ApplicationLike extends PartialFunction[Request, Response] with Routing with Intrinsics {
    implicit val intrinsicValues: IntrinsicValues = this

    def isDefinedAt(request: Request) = routes exists {
      case Route(method, rule, _) if (method is request.method) && (rule (request) isDefined) => true
      case _ => false
    }

    private def routesFor(request: Request) = routes collect {
      case route @ Route (method, rule, _) if method is request.method => (route, rule (request))
    } collect {
      case (r, Some(i)) => (r, i)
    }

    def execute(route: Route, request: Request, invocation: Invocation): Response =
      using (RequestContext (request, invocation)) (route action())

    // This does not take http method into consideration at the moment!
    // this blows up if findRoutes returns Nil. That implies that isDefinedAt
    // returned a false positive otoh
    def apply(request: Request) = routesFor (request) match {
      case (route, invocation) :: xs => execute (route, request, invocation)
    }
  }

  trait Application extends ApplicationLike with WwwFormHandling

  object NotFound extends Application {
    override def isDefinedAt(request: Request) = true
    override def apply(request: Request) = StatusReply(404, request.path + " not found!")
  }
}