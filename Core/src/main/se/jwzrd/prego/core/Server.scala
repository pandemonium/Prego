package se.jwzrd.prego.core

import com.apple.concurrent.Dispatch
import com.apple.concurrent.Dispatch.Priority
import java.net.{Socket, ServerSocket}
import annotation.tailrec
import io.Source
import java.lang.String
import collection.{Iterator, Seq}
import java.util.concurrent.Executor
import java.io._
import xml.NodeSeq
import Iterator.continually


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

    def repeatedly(body: => Unit) = eternally(body)

    @tailrec
    private def eternally(body: => Unit): Unit = {
      body; eternally(body)
    }

    def concurrently(body: => Unit)(implicit executor: Executor) =
      executor execute body
  }

  trait Http { self: Parsing with ResourceUsage =>
    val LineSeparator = "\r\n"
    val HeaderNameValueSeparator = ": "
    val Empty = ""
    val RequestItemSeparator = ' '

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

      Response (status, message, headers, Some(body))
    }

    def emptyResponse(status: Int = 200, message: String = "Ok") =
      Response (status, message, Map empty, None)

    case class Response(status: Int,
                        statusMessage: String,
                        headers: Map[String, String],
                        body: Option[MessageBody])
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

  object HttpServer extends Control with Parsing with ResourceUsage with Http {
    lazy val serverSocket = new ServerSocket(9090)
    implicit lazy val threaded = Dispatch.getInstance getAsyncExecutor(Priority NORMAL)

    def run = continually (serverSocket accept) foreach { s =>
      concurrently {
        dispatch(Connection(s))
      }
    }

    private def dispatch(connection: Connection) {
      val request = connection readRequest

      println(request)

      if (request.path == "/foo")
        connection sendResponse(404, "Not found you fat bastard")
      else
        connection sendResponse(<h1>Hello, world</h1>)
    }

    case class Connection(socket: Socket) extends ResourceUsage { Http =>
      def readRequest(): Request = {
        val s = Source fromInputStream(socket getInputStream)

        val (method, path, version) = parseRequestLine (parseLine (s))
        val lines = continually (parseLine (s))
        val headers = Map() ++ parseHeaders (lines)

        new Request(method, path, version, headers, s)
      }

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

      def write(response: Response) = response match {
        case Response (status, message, headers, content) =>
          withResource (socket) { os =>
            implicit val sink = new OutputStreamWriter (os)
            writeStatusLine (status, message)
            writeHeaders (headers)
            writeLine ()
            sink flush()

            content foreach (_.writeContentTo (os))
            os flush()
          }
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

  def main(args: Array[String]) = HttpServer run
}