package se.jwzrd.prego.core.server.http

import se.jwzrd.prego.core.server.http.Application.{Route, ApplicationExecution}
import java.util.concurrent.atomic.AtomicLong
import collection.mutable.WeakHashMap

/**
 * @author Patrik Andersson <pandersson@gmail.com>
 */
trait Session

private object SessionRegistrar {
  val base = new AtomicLong(System currentTimeMillis)
  val store = new WeakHashMap[String, Session]

  def lookup(id: String) = store get id map { (id, _) }

  def newSession = {
    val session = new Session {}
    val id = generate

    store put(id, session)

    (id, session)
  }

  def generate: String = (base addAndGet 1) toString
}

// VCQ7804N 
trait SessionHandling extends ApplicationExecution { ApplicationLike =>
  override abstract def execute(route: Route,
                                request: Request,
                                invocation: Invocation): Response = {
    val SR = SessionRegistrar

    invocation.cookies find (_.name == "PSID") match {
      case Some(sessionCookie) =>
      case _ =>
    }

    val (id, session) = SR synchronized {
      SR lookup "apan" getOrElse (SR newSession)
    }

    // There are threading issues here!

    // find Cookie by "PSID"
    //   lookup Session instance and add to Invocation
    
    // not found OR no session remaining:
    //   generate PSID
    //   create Session instance
    //   add to SessionRegistrar
    //   add cookie PSID=<id>

    // All sessions held on using WeakReference

    super.execute(route, request, invocation)    
  }
}