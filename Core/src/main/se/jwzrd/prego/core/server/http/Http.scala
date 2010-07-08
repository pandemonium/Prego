package se.jwzrd.prego.core.server.http

import se.jwzrd.prego.core.server.Parsing
import java.io.{InputStream, Writer}
import io.Source
import Iterator.continually

/**
 * @author Patrik Andersson <pandersson@gmail.com>
 */
trait Http { self: Parsing =>
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

}