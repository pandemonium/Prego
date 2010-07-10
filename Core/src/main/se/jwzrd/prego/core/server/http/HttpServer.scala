package se.jwzrd.prego.core.server.http

/*import com.apple.concurrent.Dispatch
import com.apple.concurrent.Dispatch.Priority*/
import java.lang.String
import Iterator.continually
import java.net._
import collection.immutable.Map
import scala.{Application => _}
import Application._
import HttpMethod._
import Response._
import se.jwzrd.prego.core.server.{Parsing, ResourceUsage, Control}
import java.util.concurrent.{Executors, Executor}

/**
 * @author Patrik Andersson <pandersson@gmail.com>
 */

object HttpServer {
  def apply(address: SocketAddress, processor: RequestProcessor) = {
    val ss = new ServerSocket
    ss.bind (address)

//    new HttpServer (ss, processor, Dispatch.getInstance getAsyncExecutor (Priority NORMAL))
    new HttpServer (ss, processor, Executors newCachedThreadPool)
  }

  class HttpServer(val serverSocket: ServerSocket,
                   val processor: RequestProcessor,
                   implicit val executor: Executor) extends Control {
    def run = for (c <- continually (Connection (serverSocket accept)))
      concurrently (dispatch (c))

    def stop = serverSocket close

    protected def dispatch(connection: Connection) =
      connection.send (processor (connection readRequest))

    case class Connection(socket: Socket) extends ResourceUsage with Http with Parsing {
      def readRequest = parseRequest (socket getInputStream)

      def send(response: Response): Unit = withResource (socket) {
        response (_)
      }

      def send[A <% MessageBody](body: A): Unit =
        send (Content (body))
    }
  }
}