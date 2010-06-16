package se.jwzrd.prego.core

import com.apple.concurrent.Dispatch
import com.apple.concurrent.Dispatch.Priority
import annotation.tailrec
import io.Source
import java.lang.String
import collection.{Iterator, Seq}
import java.util.concurrent.Executor
import java.io._
import xml.NodeSeq
import Iterator.continually
import java.net._
import collection.immutable.Map
import util.DynamicVariable

/**
 * Inheritance and partitioning in traits is fucked.
 * 
 * @author Patrik Andersson <pandersson@gmail.com>
 */
object Server {

  trait Control {
    implicit def byNameIsRunnable(body: => Unit): Runnable = new Runnable {
      def run = body
    }

    def run: Unit

    def stop: Unit

    def repeatedly(body: => Unit) = eternally(body)

    @tailrec
    private def eternally(body: => Unit): Unit = {
      body; eternally(body)
    }

    def concurrently(body: => Unit)(implicit executor: Executor) =
      executor execute body
  }

  trait Http extends Parsing {
    val LineSeparator = "\r\n"
    val HeaderNameValueSeparator = ": "
    val Empty = ""
    val RequestItemSeparator = ' '

    def parseRequest(inputStream: InputStream): Request = {
      val s = Source fromInputStream (inputStream)

      val (method, path, version) = parseRequestLine (parseLine (s))
      val lines = continually (parseLine (s))
      val headers = Map() ++ parseHeaders (lines)

      new Request(method, path, version, headers, s)
    }

    def parseRequestLine(line: String) = line split RequestItemSeparator match {
      case Array(a, b, c) => (a, b, c)
    }

    def parseHeaders(lines: Iterator[String]) =
      lines takeWhile isHeaderLine map parseHeader

    private def parseHeader(line: String) = line split HeaderNameValueSeparator match {
      case Array(name, value) => (name, value trim)
    }

    private def isHeaderLine(line: String) =
      !(line trim() isEmpty)

    def parseLine(source: Iterator[Char]) =
      (parseUntil(source)(LineSeparator) right) getOrElse Empty

    def writeStatusLine(status: Int, message: String)(implicit sink: Writer) =
      writeLine ("HTTP/1.1 " + status + " " + message)

    def writeHeaders(headers: Map[String, String])(implicit sink: Writer) =
      headers foreach writeHeader

    def writeHeader(header: (String, String))(implicit sink: Writer) = header match {
      case (k, v) => writeLine(k + ": " + v)
    }

    def writeLine(line: String = Empty)(implicit sink: Writer) =
      sink write(line + LineSeparator)

    trait MessageBody extends (OutputStream => Unit) {
      val contentType: String
      val contentLength: Int
    }

    trait TextBody extends MessageBody {
      protected val content: String
      val contentLength = content length
      val contentType = "text/plain"

      def apply(sink: OutputStream) = {
        val writer = new OutputStreamWriter (sink)
        writer write (content)
        writer flush()
      }
    }

    implicit def xmlCanBeMessageBody(xml: NodeSeq): MessageBody = new TextBody {
      lazy val content = xml toString
      override val contentType = "text/xml"
    }

    implicit def xmlCanBeGenericResponse(xml: NodeSeq): Response =
      Content(xml)

    trait Routing {
      var routes: Map[String, () => Response] = Map()

      // This method is going to have to take the designated HTTP method too
      protected def add(pattern: String, method: HttpMethod)(handler: => Response): Unit =
        routes += (pattern -> (() => handler))

      import HttpMethod._

      def get(pattern: String)(handler: => Response): Unit =
        add(pattern, Get)(handler)

      def head(pattern: String)(handler: => Response): Unit =
        add(pattern, Head)(handler)

      def put(pattern: String)(handler: => Response): Unit =
        add(pattern, Put)(handler)

      def post(pattern: String)(handler: => Response): Unit =
        add(pattern, Post)(handler)

      def option(pattern: String)(handler: => Response): Unit =
        add(pattern, Option)(handler)

      def delete(pattern: String)(handler: => Response): Unit =
        add(pattern, Delete)(handler)
    }

    case class RequestContext(val request: Request)

    trait Intrinsics {
      val state = new DynamicVariable[Option[RequestContext]](None)

      def request: Option[Request] =
        state.value map (_.request)

      def using[A](rc: RequestContext)(thunk: => A): A =
        state.withValue (Some(rc))(thunk)
    }

    trait Application extends PartialFunction[Request, Response] with Routing with Intrinsics {
      // This isn't entirely true!
      def isDefinedAt(request: Request) =
        routes.isDefinedAt(request path)

      // This isn't entirely true!
      def apply(request: Request) = using(RequestContext (request)) {
        routes (request path)()
      }
    }

    sealed trait HttpMethod {
      val name: String

      def is(name: String) =
        this.name == name
    }

    object HttpMethod {
      object Get extends Impl("GET")
      object Head extends Impl("HEAD")
      object Put extends Impl("PUT")
      object Post extends Impl("POST")
      object Option extends Impl("OPTION")
      object Delete extends Impl("DELETE")

      private val methods = List(Get, Head, Put, Post, Option, Delete)

      abstract class Impl(val name: String) extends HttpMethod
    }

/*
    GOAL(ish):
    class ConcreteApplication extends Intrinsics with Routing {
      get("/foo") {
        <h1>Hello, foo</h1>
      }
    }
*/

    object StatusReply extends ((Int, String) => Response) {
      def apply(status: Int, message: String) = 
        new Response(status, message, Map empty, None)
    }

    object Redirect extends (String => Response) {
      def apply(location: String) =
        new Response(301, "Moved Permanently", Map("Location" -> location), None)
    }

    object Content {
      def apply[A <% MessageBody](body: A,
                                  headers: Map[String, String] = Map(),
                                  status: Int = 200,
                                  message: String = "Ok") =
        new Response(status, message, headers ++ contentHeaders (body), Some(body))

      def contentHeaders(body: MessageBody): Map[String, String] =
        Map ("Content-Type" -> (body contentType),
             "Content-Length" -> (body contentLength).toString)
    }

    class Response(status: Int,
                          message: String,
                          headers: Map[String, String],
                          body: Option[MessageBody]) extends Http {
      def apply(outputStream: OutputStream): this.type = {
        implicit val sink = new OutputStreamWriter (outputStream)
        writeStatusLine (status, message)
        writeHeaders (headers)
        writeLine ()
        sink flush ()

        body foreach (_ (outputStream))
        outputStream flush ()

        this
      }
    }

    class Request(val method: String,
                  val path: String,
                  val httpVersion: String,
                  val headers: Map[String, String],
                  val data: Source) {
      override def toString =
        method + "|" + path + "|" + httpVersion + "|" + headers + "|"
    }
  }

  trait Parsing {
    type ParseResult = String Either String

    def parseUntil(source: Iterator[Char])(delimitor: Seq[Char]): ParseResult = {
      if (!delimitor.isEmpty) {
        val head = delimitor head
        val taken = source takeWhile (head !=) toArray

        if (prefixMatches(source)(delimitor tail))
          emit(taken)
        else
          expected(delimitor)
      } else
        emit(source toArray)
    }

    def emit(source: Array[Char]) =
      Right(new String(source toArray))

    def expected(what: Seq[Char]) =
      Left("expected [" + what + "]")

    def prefixMatches(source: Iterator[Char])(limit: Seq[Char]): Boolean =
      if (source.hasNext && !limit.isEmpty) {
        val tail = limit.tail
        limit.head == source.next && (tail.isEmpty || prefixMatches(source)(tail))
      } else
        limit.isEmpty    
  }

  /**
   * This thing is not good.
   */
  trait ResourceUsage {
    type Streamed = {
      def getOutputStream(): OutputStream
    }

    def withResource(resource: Streamed)(using: OutputStream => Unit): Unit = {
      val os = resource getOutputStream

      try using(os)
      finally {
        os flush; os close
      }
    }
  }

  object HttpServer extends (SocketAddress => Control) {
    def apply(address: SocketAddress) = {
      val ss = new ServerSocket
      ss.bind(address)

      new HttpServer(ss)
    }
  }

  class HttpServer(val serverSocket: ServerSocket) extends Control with ResourceUsage with Http {
    implicit lazy val threaded = Dispatch.getInstance getAsyncExecutor(Priority NORMAL)

    def run = for (c <- continually (Connection (serverSocket accept)))
      concurrently (dispatch(c))

    def stop = serverSocket close

    protected def dispatch(connection: Connection) {
      val request = connection readRequest

      println(request)

      if (request.path == "/foo")
        connection send StatusReply(404, "Not found!")
      else
        connection send <h1>Hello, world</h1>
    }

    case class Connection(socket: Socket) extends ResourceUsage { Http =>
      def readRequest = parseRequest(socket getInputStream)

      def send(response: Response): Unit = withResource (socket) {
        response(_)
      }

      def send[A <% MessageBody](body: A): Unit =
        send (Content(body))
    }
  }

  def main(args: Array[String]) =
    HttpServer(new InetSocketAddress(9090)) run
}