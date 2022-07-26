package io.github.edadma.recognizer

import scala.language.implicitConversions

trait CharRecognizer[W] extends Recognizer[W, Char] {

  implicit def str(s: String): Pattern = Match(s.toList)

  def kw(s: String): Pattern = s ~ not(alphanum) ~ ws

  def sym(s: String): Pattern = string(s) ~ ws

  val alpha: Pattern = clas(_.isLetter)
  val alphanum: Pattern = clas(_.isLetterOrDigit)
  val digit: Pattern = clas(_.isDigit)
  val digits: Pattern = rep1(digit)
  val whitespace: Pattern = clas(_.isWhitespace)
  val ws: Pattern = rep(whitespace)
  val ws1: Pattern = rep1(whitespace)
  val ident: Pattern = string((alpha | '_') ~ rep(alphanum | '_')) ~ ws
  val number: Pattern =
    (rep(digit) ~ '.' ~ digits | digits ~ '.') ~
      opt((elem('e') | 'E') ~ opt(elem('+') | '-') ~ digits) |
      digits

  def string(p: Pattern): Pattern = capture(p)(_.listElem(_).mkString)

}
