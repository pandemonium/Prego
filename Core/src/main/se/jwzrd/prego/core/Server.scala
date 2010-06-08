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

/**
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

  trait Http { self: Parsing =>
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

    private def isHeaderLine(line: String) = !(line trim() isEmpty)

    def parseLine(source: Iterator[Char]) =
      (parseUntil(source)(LineSeparator) right) getOrElse Empty
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

  object HttpServer extends Control with Parsing with Http {
    lazy val serverSocket = new ServerSocket(9090)
    lazy val connectedClients: Iterator[Socket] = Iterator continually (serverSocket accept)
    implicit lazy val threaded = Dispatch.getInstance.getAsyncExecutor(Priority.NORMAL)

    def run = connectedClients foreach { s =>
      concurrently {
        dispatch(Connection(s))
      }
    }

/*
    def run = repeatedly {
      val socket = serverSocket accept

      concurrently {
        dispatch (Connection(socket))
      }
    }
*/

    implicit def xmlIsResponseLike(xml: NodeSeq): ResponseLike = new ResponseLike {
      // this is awkward
      def writeContentTo(sink: OutputStream) = {
        val writer: OutputStreamWriter = new OutputStreamWriter (sink)
        writer write(xml toString)
        writer flush()
      }

      val hasContent = true
      val statusMessage = "Ok"
      val status = 200
      val contentType = "text/xml"
    }

    private def dispatch(connection: Connection) {
      println(connection readRequest)

      connection.sendResponse(<h1>Hello, world</h1>)
    }

    case class Connection(socket: Socket) extends ResourceUsage { Http =>
      def readRequest(): Request = {
        val s = Source fromInputStream(socket getInputStream)

        val (method, path, version) = parseRequestLine (parseLine (s))
        val lines = Iterator continually parseLine(s)
        val headers = Map() ++ parseHeaders (lines)

        new Request(method, path, version, headers, s)
      }

      def sendResponse[A <% ResponseLike](response: A) = 
        (if (response hasContent)
          write(responseWithContent) _
         else
          write(emptyResponse) _) (response)

      def responseWithContent[A <% ResponseLike](responseBody: A): ResponseMessage = {
        val buffer = new ByteArrayOutputStream (4096)
        responseBody writeContentTo buffer

        val headers = Map ("Content-Type" -> (responseBody contentType),
                           "Content-Length" -> (buffer size).toString)

        ResponseMessage (responseBody status,
                         responseBody statusMessage,
                         headers,
                         Some (buffer toByteArray))
      }

      def emptyResponse[A <% ResponseLike](responseBody: A) =
        ResponseMessage (responseBody status,
                         responseBody statusMessage,
                         Map (),
                         None)

      type ResponseBuilder = ResponseLike => ResponseMessage

      def write(build: ResponseBuilder)(response: ResponseLike) = build (response) match {
        case ResponseMessage (status, message, headers, content) =>
          withResource (socket) { os =>
            implicit val sink = new OutputStreamWriter (os)
            writeStatusLine (status, message)
            writeHeaders (headers)
            sink.flush()
            
            content foreach os.write
            sink.flush()
          }
      }

      def writeStatusLine(status: Int, message: String)(implicit sink: Writer) =
        writeLine ("HTTP/1.1 " + status + " " + message)

      def writeHeaders(headers: Map[String, String])(implicit sink: Writer) = {
        headers foreach writeHeader; writeLine ()
      }

      def writeHeader(header: (String, String))(implicit sink: Writer) = header match {
        case (k, v) => writeLine(k + ": " + v)
      }

      def writeLine(line: String = Empty)(implicit sink: Writer) =
        sink write(line + LineSeparator)
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

    case class ResponseMessage(status: Int,
                               statusMessage: String,
                               headers: Map[String, String],
                               content: Option[Array[Byte]])

    trait ResponseLike {
      val contentType: String
      val status: Int
      val statusMessage: String
      val hasContent: Boolean

      def writeContentTo(sink: OutputStream): Unit
    }

    class Request(val method: String,
                  val path: String,
                  val httpVersion: String,
                  val headers: Map[String, String],
                  val data: Source) {
      override def toString = {
//        if (data hasNext) println("Hi")   // Hangs because there's nothing more to read
        
        method + "|" + path + "|" + httpVersion + "|" + headers + "|"/* + (if (data.hasNext) new String(data toArray) else "")*/
      }
    }
  }

  def main(args: Array[String]) = HttpServer run
}