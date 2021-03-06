package se.jwzrd.prego.core.server.http

import se.jwzrd.prego.core.server.Parsing
import xml.{Text, NodeSeq}
import java.io.{PrintWriter, StringWriter, OutputStream, OutputStreamWriter}
import java.util.Date

/**
 * @author Patrik Andersson <pandersson@gmail.com>
 */

// change to support multiple headers with the same name
case class Response(val status: Int,
                    val message: String,
                    val headers: Map[String, String],
                    val body: Option[MessageBody]) extends Http with Parsing {
  def apply(outputStream: OutputStream): this.type = {
    implicit val sink = new OutputStreamWriter(outputStream)
    writeStatusLine(status, message)
    writeHeaders(headers)
    writeLine()
    sink flush ()

    body foreach (_(outputStream))
    outputStream flush ()

    this
  }

  override def toString =
    "status: " + status +
    "; message: " + message +
    "; headers: " + headers
}


// move these to the http package object
object Response {
  implicit def xmlCanBeGenericResponse(xml: NodeSeq): Response = Content (xml)
  implicit def xmlCanBeMessageBody(xml: NodeSeq): MessageBody = new XmlBody(xml)
}

// This cannot be tied to HTML so how should I solve this?
object InternalServerError {
  def apply(message: String) =
    template(Text(message))

  def apply(t: Throwable) = {
    t printStackTrace

    template (<pre>{stacktrace (t)}</pre>)
  }

  def template(content: NodeSeq): Response =
    <html>
      <body>
        <h1>500 Internal server error</h1>
        {content}
      </body>
    </html>

  def stacktrace(t: Throwable) = {
    val sw = new StringWriter
    t printStackTrace new PrintWriter (sw)
    sw toString
  }
}

trait ServerHeaders {
  type Headers = Map[String, String]

  def serverHeaders: Headers =
    Map("Date" -> new Date().toString,
        "Server" -> "Prego/0.1")
}

object StatusReply extends ServerHeaders {
  def apply(status: Int, message: String) =
    new Response (status, message, serverHeaders, None)
}

object Redirect extends ServerHeaders {
  def apply(location: String) =
    new Response (301, "Moved Permanently", serverHeaders + ("Location" -> location), None)
}

object Content extends ServerHeaders {
  def apply[A <% MessageBody](body: A,
                              headers: Headers = Map (),
                              status: Int = 200,
                              message: String = "Ok") =
    new Response (status, message, compileHeaders (contentHeaders (body), headers), Some (body))

  def compileHeaders(contentHeaders: Headers, headers: Headers) =
    contentHeaders ++ headers ++ serverHeaders

  def contentHeaders(body: MessageBody): Headers =
    Map ("Content-Type" -> (body contentType),
         "Content-Length" -> (body contentLength).toString)
}

trait MessageBody extends (OutputStream => Unit) {
  val contentType: String
  val contentLength: Long
}

trait TextBody extends MessageBody {
  protected val content: String
  val contentLength = (content length) toLong
  val contentType = "text/plain"

  def apply(sink: OutputStream) = {
    val writer = new OutputStreamWriter (sink)
    writer write (content)
    writer flush ()
  }
}

class XmlBody (val document: NodeSeq) extends TextBody {
  override lazy protected val content = document toString

  override val contentType = document match {
    case <html>{ _* }</html> => "text/html"
    case _ => "text/xml"
  }
}