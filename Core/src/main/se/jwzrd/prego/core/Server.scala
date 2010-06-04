package se.jwzrd.prego.core

import com.apple.concurrent.Dispatch
import com.apple.concurrent.Dispatch.Priority
import java.net.{Socket, ServerSocket}
import annotation.tailrec
import io.Source
import java.lang.String
import collection.{Iterator, Seq}
import java.util.concurrent.Executor

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
    implicit lazy val threaded = Dispatch.getInstance.getAsyncExecutor(Priority.NORMAL)

    def run = repeatedly {
      val socket = serverSocket accept

      concurrently {
        dispatch (Connection(socket))
      }
    }

    private def dispatch(connection: Connection) {
      println(connection readRequest)
    }

    case class Connection(socket: Socket) {
      def readRequest(): Request = {
        val s = Source fromInputStream(socket getInputStream)

        val (method, path, version) = parseRequestLine (parseLine (s))
        val lines = Iterator continually parseLine(s)
        val headers = Map() ++ parseHeaders (lines)

        new Request(method, path, version, headers, s)
      }

      def sendResponse(): Unit = {}
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