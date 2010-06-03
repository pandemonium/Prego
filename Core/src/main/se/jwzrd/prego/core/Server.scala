package se.jwzrd.prego.core

import com.apple.concurrent.Dispatch
import com.apple.concurrent.Dispatch.Priority
import java.net.{Socket, ServerSocket}
import annotation.tailrec
import io.Source
import java.lang.String
import collection.Seq

/**
 * @author Patrik Andersson <pa@king.com>
 */
object Server {
  val serverSocket = new ServerSocket(9090)
  val threaded = Dispatch.getInstance.getAsyncExecutor(Priority.NORMAL)
  
  def run = repeatedly {
    val socket = serverSocket accept

    concurrently {
      dispatch (Connection(socket))
    }
  }

  @tailrec
  private def repeatedly(body: => Unit): Unit = {
    body; repeatedly (body)
  }

  private def concurrently(body: => Unit) = threaded execute body

  private def dispatch(connection: Connection) {
    println(connection readRequest)
  }

  implicit def byNameIsRunnable(body: => Unit): Runnable = new Runnable {
    def run = body
  }

  private def parseRequestLine(line: String) = line split ' ' match {
    case Array(a, b, c) => (a, b, c)
  }

  private def parseHeaders(lines: Iterator[String]) =
    lines takeWhile isHeaderLine map parseHeader

  private def parseHeader(line: String) = line split ": " match {
    case Array(name, value) => (name, value trim)
  }

  private def isHeaderLine(line: String) = !(line trim() isEmpty)

  case class Connection(socket: Socket) {
    def readRequest(): Request = {
      val s = Source fromInputStream(socket getInputStream)

      // should not do getLines; just do takeWhile NotLineSeparator instead. That way the post
      // data won't be killed
      val lines = s getLines ("\r\n")

      val (method, path, version) = parseRequestLine (lines next)
      val headers = Map() ++ parseHeaders (lines)

      Request(method, path, version, headers, null)      
    }

    def sendResponse(): Unit = {}
  }

  case class Request(method: String,
                     path: String,
                     httpVersion: String,
                     headers: Map[String, String],
                     data: Source)

  private def takeUntil(source: Iterator[Char])(limit: Seq[Char]): Iterator[Char] Either String = {
    val head = limit head
    val taken = source takeWhile (head !=)

    print("[")
    taken foreach (print); println("]")
    print("[")
    source foreach (print); println("]")

    println("[" + (limit tail) + "]")

    if (prefixMatches(source)(limit tail))
      Left(taken)
    else
      Right("Foo")
  }

  private def prefixMatches(source: Iterator[Char])(limit: Seq[Char]): Boolean = {
/*
    source foreach (print)
    println()

    println("[" + limit + "]")
*/

    if ((source hasNext) && (limit length) >= 1)
      ((limit head) == (source next)) && (((limit length) == 1) || prefixMatches(source)(limit tail))
    else
      (!(source hasNext) && (limit isEmpty))
  }

  def main(args: Array[String]) = {
    //Server run

/*
    val b = Server.prefixMatches(" world".iterator)(" ")
    println(b)
*/

    val s = Server.takeUntil("Hello, world".iterator)(", ")
    println(s)
  }
}