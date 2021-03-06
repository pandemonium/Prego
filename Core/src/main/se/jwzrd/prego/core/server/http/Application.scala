package se.jwzrd.prego.core.server.http

import util.DynamicVariable
import collection.immutable.Map
import java.net.URLDecoder
import java.util.Date
import java.lang.{Throwable, String}

/**
 * This should be an application package object under core.
 *
 * @author Patrik Andersson <pandersson@gmail.com>
 */
object Application {
  // XXX
  private val globalState = new DynamicVariable[RequestContext](null)

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
    def cookies: Seq[Cookie]
    def cookie(name: String): Option[Cookie]
    def parameters: Map[String, String]
    def session: Session
  }

  trait Intrinsics extends IntrinsicValues {
    val intrinsicState: DynamicVariable[RequestContext]

    implicit def parameters: Map[String, String] =
      context.invocation parameters()

    def request: Request = context request

    def cookies: Seq[Cookie] = context.invocation cookies

    def cookie(name: String): Option[Cookie] = cookies find (_.name == name)

    def context = intrinsicState value

    def session: Session = context.invocation.session get

    def using[A](rc: RequestContext)(thunk: => A): A =
      intrinsicState.withValue (rc) (thunk)
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

  trait ApplicationExecution {
    def execute(route: Route, request: Request, invocation: Invocation): Response
  }

  trait ExceptionHandling extends ApplicationExecution { ApplicationLike =>
    override abstract def execute(r: Route,
                                  rq: Request,
                                  i: Invocation): Response = {
      import util.control.Exception._
      allCatch either super.execute (r, rq, i) fold (e => InternalServerError(e), identity)
    }
  }

  trait ApplicationLike extends PartialFunction[Request, Response]
                           with Routing
                           with Intrinsics
                           with ApplicationExecution {
    // XXX
    val intrinsicState = globalState
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

    def apply(request: Request) = routesFor (request) match {
      case (route, invocation) :: xs => execute (route, request, invocation)
    }
  }

  // Ideally, the intrinsics could also be mixed-in (the using() call) as an IntrinsicsHandler

  // The ordering here is vital! CookieHandling must happen before SessionHandling so it
  // must therefore be *after* in inheritance order

  // It would be nice if an Application could be made to "sit" under a path prefix
  // much like FileServer has a context path
  trait Application extends ApplicationLike
                       with WwwFormHandling
                       with SessionHandling
                       with CookieHandling
                       with ExceptionHandling

  object NotFound extends Application {
    override def isDefinedAt(request: Request) = true
    override def apply(request: Request) = StatusReply(404, request.path + " not found!")
  }
}