package io.github.edadma.recognizer

import scala.language.implicitConversions

trait CharRecognizer extends Recognizer[Char, Char] {

  implicit def str(s: String): Pattern = Match(s.toList)

  def alpha: Pattern = clas(_.isLetter)

  def alphanum: Pattern = clas(_.isLetterOrDigit)

  def digit: Pattern = clas(_.isDigit)

  def whitespace: Pattern = clas(_.isWhitespace)

  def string(p: Pattern): Pattern =
    pointer ~ p ~ pointer ~ transform(2) {
      case Seq(start, end) => start.asInstanceOf[I].listElem(end.asInstanceOf[I]).get.mkString
    }

}
