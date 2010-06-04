package se.jwzrd.prego.core

import xml.NodeSeq

/**
 * @author Patrik Andersson <pandersson@gmail.com>
 */

object Core {

  object HttpMethod extends Enumeration {
    val Get, Post, Put, Delete, Option, Head = Value
  }

  trait Parameter {
    val name: Symbol

    def read[A]: Either[String, A]
  }

  trait Request {
    def get[B](p: Parameter): Option[B]

//    def get[B](p: Parameter): Either[String, B]

    // method for reading from the request entity
  }

  trait Response[A] {
    import java.io._

    type S <: {
      def setContentLength(l: Int): Unit

      def setContentType(ct: String)

      def getWriter: Writer

      def sendRedirect(url: String): Unit

      def setStatus(s: Int): Unit

      def sendError(s: Int): Unit

      def sendError(s: Int, text: String): Unit
    }

    def commit(s: S): Unit
  }

/*
  abstract class ServletResponseAdapter extends Response {
    def commit: Unit = {}
  }
*/

  trait ResponseBuilder[A] {
    def build(source: A): Response[A]
  }

  implicit object XmlCanBeAResponse extends ResponseBuilder[NodeSeq] {
    def build(source: NodeSeq) = null//source.text
  }

  type Action[A] = Request => A

  trait Resource {
    import HttpMethod._

    private var bindings: Map[String, Action[_]] = Map()

    // this method needs to take some object that can A => HttpResult
    protected def bind[A](urlPattern: String)(method: Value)(action: => A): Unit = null
//      bindings += (urlPattern -> (() => action))

    def get[A](urlPattern: String)(action: => A) =
      bind(urlPattern)(Get)(() => action)

    def post[A](urlPattern: String)(action: => A) =
      bind(urlPattern)(Post)(() => action)

    def put[A](urlPattern: String)(action: => A) =
      bind(urlPattern)(Put)(() => action)

    def delete[A](urlPattern: String)(action: => A) =
      bind(urlPattern)(Delete)(() => action)

    def option[A](urlPattern: String)(action: => A) =
      bind(urlPattern)(Option)(() => action)

    def head[A](urlPattern: String)(action: => A) =
      bind(urlPattern)(Head)(() => action)
  }
}