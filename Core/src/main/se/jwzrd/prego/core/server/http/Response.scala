package se.jwzrd.prego.core.server.http

import java.io.{OutputStream, OutputStreamWriter}
import se.jwzrd.prego.core.server.Parsing
import xml.NodeSeq

/**
 * @author Patrik Andersson <pandersson@gmail.com>
 */
class Response(val status: Int,
               val message: String,
               val headers: Map[String, String],
               val body: Option[MessageBody]) extends Http with Parsing {
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

object Response {
  implicit def xmlCanBeGenericResponse(xml: NodeSeq): Response = Content (xml)
  implicit def xmlCanBeMessageBody(xml: NodeSeq): MessageBody = new XmlBody(xml)
}

object StatusReply extends ((Int, String) => Response) {
  def apply(status: Int, message: String) =
    new Response (status, message, Map empty, None)
}

object Redirect extends (String => Response) {
  def apply(location: String) =
    new Response (301, "Moved Permanently", Map ("Location" -> location), None)
}

object Content {
  def apply[A <% MessageBody](body: A,
                              headers: Map[String, String] = Map (),
                              status: Int = 200,
                              message: String = "Ok") =
    new Response (status, message, contentHeaders (body) ++ headers, Some (body))

  def contentHeaders(body: MessageBody): Map[String, String] =
    Map ("Content-Type" -> (body contentType),
         "Content-Length" -> (body contentLength).toString)
}

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