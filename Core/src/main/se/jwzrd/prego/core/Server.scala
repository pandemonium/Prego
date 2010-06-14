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

    trait MessageBody {
      val contentType: String
      val contentLength: Int

      def writeContentTo(sink: OutputStream): Unit
    }

    trait TextBody extends MessageBody {
      protected val content: String
      val contentLength = content length
      val contentType = "text/plain"

      def writeContentTo(sink: OutputStream) = {
        val writer = new OutputStreamWriter (sink)
        writer write (content)
        writer flush()
      }
    }

    implicit def xmlCanBeMessageBody(xml: NodeSeq): MessageBody = new TextBody {
      lazy val content = xml toString
      override val contentType = "text/xml"
    }

    def responseWithContent[A <% MessageBody](status: Int, message: String)(body: A): Response = {
      val headers = Map ("Content-Type" -> (body contentType),
                         "Content-Length" -> (body contentLength).toString)

      new Response (status, message, headers, Some(body))
    }

    def emptyResponse(status: Int = 200, message: String = "Ok") =
      new Response (status, message, Map empty, None)

    trait ResponseControl {
      protected def statusReply(status: Int, message: String): Unit
      protected def redirect(target: String): Unit
    }

    trait Routing {
      protected def route[A <% MessageBody](pattern: String)(handler: => A): Unit
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

        body foreach (_.writeContentTo (outputStream))
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
    def apply(address: SocketAddress) =
      new HttpServer(address)
  }

  class HttpServer(val address: SocketAddress) extends Control with ResourceUsage with Http {
    lazy val serverSocket = {
      val ss = new ServerSocket
      ss.bind(address)

      ss
    }

    implicit lazy val threaded = Dispatch.getInstance getAsyncExecutor(Priority NORMAL)

    def run = continually (serverSocket accept) foreach { s =>
      concurrently {
        dispatch(Connection(s))
      }
    }

    def stop =
      serverSocket.close

    private def dispatch(connection: Connection) {
      val request = connection readRequest

      println(request)

      if (request.path == "/foo")
        connection sendResponse(404, "Not found you fat bastard")
      else
        connection sendResponse(<h1>Hello, world</h1>)
    }

    case class Connection(socket: Socket) extends ResourceUsage { Http =>
      def readRequest =
        parseRequest(socket getInputStream)

      def sendResponse[A <% MessageBody](body: A, status: Int = 200, message: String = "Ok"): Unit =
        sendResponse(Some(body), status, message)

      def sendResponse(status: Int, message: String): Unit =
        sendResponse(None, status, message)

      protected def sendResponse[A <% MessageBody](body: Option[A],
                                                   status: Int,
                                                   message: String) = body match {
        case Some(x) =>
          write(responseWithContent(status, message)(x))
        case None =>
          write(emptyResponse(status, message))
      }

      def write(response: Response) = withResource (socket) {
        response(_)
      }
    }
  }

  def main(args: Array[String]) = 
    HttpServer(new InetSocketAddress(9090)) run
}