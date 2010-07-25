package se.jwzrd.prego.core.server.http

import se.jwzrd.prego.core._
import server.http.Application.{Route, ApplicationExecution}
import java.util.concurrent.atomic.AtomicLong
import collection.mutable.{MapLike, WeakHashMap}
import java.lang.String

/**
 * @author Patrik Andersson <pandersson@gmail.com>
 */

// Do I want to be able to have an Application global special Session type where
// the application can sign up to control instantiation and tear down?

// Will Map be sufficient?

// This thing needs to override equals and hashCode to be meaningful if non-constant
// keys are to be used.

// Could the keys actually persist an instance of the Manifest?
trait Key {
  type Value

  implicit def anyToValue(any: Any): Value =
    any.asInstanceOf[Value]

  def read(implicit values: Map[Any, Any]): Value =
    values(this)
}

trait Session extends Iterable[(Any, Any)] {
  implicit var store = Map[Any, Any]()

  def iterator = store iterator

  def apply[K <: Key](key: K): K#Value = key read

  def update[K <: Key, V <: K#Value](key: K, value: V): Unit =
    store += (key -> value)
}

trait SessionRegistrarLike {
  val base = new AtomicLong(System currentTimeMillis)
  val store = new WeakHashMap[String, Session]

  type Lookup = (String, Session)

  def lookup(id: String): Option[Lookup] =
    store get id map { (id, _) }

  def createNew: Lookup = {
    val id = generate
    val session = new Session {}

    store(id) = session

    (id, session)
  }

  def generate: String = (base addAndGet 1) toString
}

private object SessionRegistrar extends SessionRegistrarLike

trait SessionHandling extends ApplicationExecution { ApplicationLike =>
  private val SessionIdCookie = "PSID"

  def findCookieSessionId(invocation: Invocation) =
    invocation.cookies find (_.name == SessionIdCookie) map (_.value)


      // There are threading issues here!

      // find Cookie by "PSID"
      //   lookup Session instance and add to Invocation

      // not found OR no session remaining:
      //   generate PSID
      //   create Session instance
      //   add to SessionRegistrar
      //   add cookie PSID=<id>

      // All sessions held on using WeakReference

  import SessionRegistrar._

  // Is this line really working?
  private def lookupOrCreateSession(cookieSessionId: Option[String]) =
    cookieSessionId flatMap lookup getOrElse createNew

  /*
    There are threading problems here. Two concurrent requests where neither passes
    the PSID cookie will both trigger the creation of a new Session, both responding
    with Set-Cookie PSID <the new id>

    Synchronizing on SessionRegistrar isn't going to help this because that is mearly
    going to serialize processing of all concurrent requests. There's still going to
    be two sessions but they'll be created back-to-back instead.
   */
  override abstract def execute(route: Route, request: Request, invocation: Invocation): Response = {
    val cookieSessionId = findCookieSessionId (invocation)
    val (sessionId, session) = lookupOrCreateSession (cookieSessionId)
    val response = super.execute (route, request, invocation copy (session = Some (session)))

    cookieSessionId filter (sessionId ==) map { _ =>
      response
    } getOrElse {
      response set Cookie (SessionIdCookie, sessionId)
    }
  }
}