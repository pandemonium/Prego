package se.jwzrd.prego.core.server.http

import io.Source
import util.matching.Regex

/**
 * @author Patrik Andersson <pandersson@gmail.com>
 */
class Request(val method: String,
              val path: String,
              val httpVersion: String,
              val headers: Map[String, String],
              val data: Source) {
  override def toString =
    method + "|" + path + "|" + httpVersion + "|" + headers + "|"
}

trait Part {
  val name: String
}

trait ParameterLike {
  val ParameterExpression: Regex

  def apply(name: String): ParameterPart

  def unapply(source: String): Option[ParameterPart] = source match {
    case ParameterExpression (name) => Some (this (name))
    case _ => None
  }
}

object OptionalParameter extends ParameterLike {
  val ParameterExpression = """\[:(.*)\]""".r

  def apply(name: String) = new ParameterPart (name, true)
}

object Parameter extends ParameterLike {
  val ParameterExpression = """:(.*)""".r

  def apply(name: String) = new ParameterPart (name)
}

class ParameterPart(val name: String, val optional: Boolean = false) extends Part {
  override def toString = if (optional) "[:" + name + "]" else ":" + name
}

object PathPart {
  def unapply(source: String): Option[PathPart] = Some (PathPart (source))

  def apply(name: String) = new PathPart (name)
}

class PathPart(val name: String) extends Part {
  override def toString = name
}

object Expression {
  private def parse(s: String) = s split "/" collect {
    case OptionalParameter (p) => p
    case Parameter (p) => p
    case PathPart (p) => p
  }

  // foo/bar/:quux/[:baz]/:lol
  def apply(text: String) = new Expression (parse (text))
}

// -> Expression instead? Expression can have an evaluate: Result
class Expression(val expression: Seq[Part]) {
  def evaluate(input: String): Option[Invocation] =
    Evaluator (expression, input)

  override def toString = expression mkString "\\"
}

object Evaluator {
  private def evaluateInput(expression: Seq[Part], input: Seq[String]): Boolean =
    if (input.length == expression.length)
      expression zip input map {
        case (p: PathPart, b) => p.name == b
        case (p: ParameterPart, b) if !p.optional => !b.isEmpty
        case (p: ParameterPart, b) if p.optional => true
      } forall (true ==)
    else false

  def apply(expression: Seq[Part], input: String) = {
    val parts = input split "/"

    if (evaluateInput (expression, parts))
      Some (new Invocation (expression, parts))
    else
      None
  }
}

// Ideally, this thing would contain the Request aswell
class Invocation(val expression: Seq[Part],
                 val input: Seq[String]) {
  lazy val parsedParameters: Map[String, String] = Map () ++ parseParameters

  protected def parseParameters = expression zip input collect {
    case (p: ParameterPart, b) if !b.isEmpty => (p.name, b)
  }

  def parameters(defaultValues: Map[String, String] = Map ()): Map[String, String] =
    defaultValues ++ parsedParameters
}