package se.jwzrd.prego.core.server.http

import se.jwzrd.prego.core.server.http.Application.Application
import java.io.{FileInputStream, OutputStream, File}
import java.nio.channels.{Channels}
import javax.activation.MimetypesFileTypeMap

/**
 * @author Patrik Andersson <pandersson@gmail.com>
 */

object FileServer {
  def apply(root: String, mappings: Map[String, String]) =
    new FileServer (root, mappings)
}

class FileServer (val root: String,
                  val mappings: Map[String, String]) extends Application {
  // request.path starsWith root
  // tail after root has a prefix among the mappings
  // a file (not directory - until directory listings is implemented!) exists at:
  //   context-path / virtual-root / virtual-relative
  //
  // As a safety measure - the served file must be a child to the fsPath
  // backing the requested virtual directory
  override def isDefinedAt(request: Request) =
    ((request path) startsWith root) &&
    (lookup(request path) map (_ isFile) getOrElse false)

  /**
   * Lookup the File behind a virtual path
   */
  private def lookup(virtualPath: String) = {
    val contextRelative = virtualPath substring root.length

    // This linear-searches a Map, that is not ideal
    mappings find {
      case (virtualRoot, fileSystemPath)
        if contextRelative startsWith virtualRoot => true
      case _ => false
    } map {
      case (virtualRoot, fileSystemPath) =>
        new File (fileSystemPath,
                  contextRelative substring virtualRoot.length)
    }
  }

  // find file
  // determine what mine-type to reply in Content-Type
  // set Content-Type to file length
  // send file
  override def apply(request: Request) =
    Content (new FileBody (lookup (request path) get))

  trait MimeTypeLookup {
    lazy val fileTypeMap = {
      val mftm = new MimetypesFileTypeMap

      // Add to configuration someplace
      mftm addMimeTypes "text/css css"
      mftm addMimeTypes "application/javascript js"

      mftm
    }

    def mimeType(file: File) =
      fileTypeMap getContentType file
  }

  class FileBody (val file: File) extends MessageBody with MimeTypeLookup {
    val contentLength = file length
    val contentType = mimeType (file)

    def apply(sink: OutputStream) = {
      val channel = new FileInputStream (file) getChannel

      try {
        channel transferTo(0, contentLength, Channels newChannel sink)
      } finally {
        channel close
      }
    }
  }
}