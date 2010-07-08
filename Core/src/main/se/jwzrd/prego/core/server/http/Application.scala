package se.jwzrd.prego.core.server.http

import util.DynamicVariable

object Application {
  import HttpMethod._

  object Module {
    def apply(xs: Application*) = new Composition {
      val applications = xs
    }
  }

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
                   pathPredicate: Expression,
                   handler: () => Response)

  trait Origin {
    def routeTo(handler: => Response): Route

    def ==> (handler: => Response)(implicit add: Route => Unit): Unit =
      add (routeTo (handler))
  }

  implicit def httpMethodCanBeRouteSource(method: HttpMethod): RouteSource =
    new RouteSource (method)

  class RouteSource(val method: HttpMethod) {
    def apply(pattern: String) = new Origin {
      def routeTo(handler: => Response) =
        Route (method, Expression (pattern), () => handler)
    }
  }

  sealed trait HttpMethod

  object HttpMethod {
    object GET extends HttpMethod
    object HEAD extends HttpMethod
    object PUT extends HttpMethod
    object POST extends HttpMethod
    object OPTION extends HttpMethod
    object DELETE extends HttpMethod
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

  trait Application extends PartialFunction[Request, Response] with Routing with Intrinsics {
    protected implicit val intrinsicValues: IntrinsicValues = this

    // This does not take http method into consideration at the moment!
    def isDefinedAt(request: Request) = routes exists { r =>
        r.pathPredicate evaluate (request path) isDefined
    }

    protected def findRoutes(request: Request): Seq[(Route, Invocation)] = routes collect {
      case r @ Route (_, matching, handler) => (r, matching evaluate(request path))       
    } collect {
      case (r, Some(i)) => (r, i)
    }

    // This does not take http method into consideration at the moment!
    // this blows up if findRoutes returns Nil. That implies that isDefinedAt
    // returned a false positive otoh
    def apply(request: Request) = findRoutes (request) match {
      case (route, invocation) :: xs => using (RequestContext (request, invocation)) {
        route.handler()
      }
    }
  }

  object NotFound extends Application {
    override def isDefinedAt(request: Request) = true
    override def apply(request: Request) = StatusReply(404, request.path + " not found!")
  }
}