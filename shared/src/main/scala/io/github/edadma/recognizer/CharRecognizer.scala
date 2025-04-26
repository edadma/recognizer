package io.github.edadma.recognizer

import scala.language.implicitConversions

/** A specialized recognizer trait for character-based input streams.
  *
  * This trait extends [[Recognizer]] to provide convenient patterns and utilities for working with character input,
  * such as string matching, whitespace handling, and common character classifications.
  *
  * @tparam W
  *   the wrapped value type
  */
trait CharRecognizer[W] extends Recognizer[W, Char] {

  /** Implicitly converts a string to a pattern that matches that exact string.
    *
    * @param s
    *   the string to match
    * @return
    *   a pattern that matches the given string
    */
  implicit def str(s: String): Pattern = Match(s.toList)

  /** Creates a pattern that matches a keyword followed by a non-alphanumeric character and whitespace.
    *
    * This is useful for matching keywords in programming languages where keywords must be followed by something other
    * than a letter, digit, or underscore.
    *
    * @param s
    *   the keyword string to match
    * @return
    *   a pattern matching the keyword followed by a non-alphanumeric and whitespace
    */
  def kw(s: String): Pattern = s ~ not(alphanum) ~ ws

  /** Creates a pattern that matches a symbol followed by whitespace.
    *
    * This is useful for matching syntactic elements like operators or punctuation in programming languages.
    *
    * @param s
    *   the symbol string to match
    * @return
    *   a pattern matching the symbol followed by whitespace
    */
  def sym(s: String): Pattern = string(s) ~ ws

  /** Pattern that matches any alphabetic character.
    */
  val alpha: Pattern = clas(_.isLetter)

  /** Pattern that matches any alphanumeric character (letter or digit).
    */
  val alphanum: Pattern = clas(_.isLetterOrDigit)

  /** Pattern that matches any digit character.
    */
  val digit: Pattern = clas(_.isDigit)

  /** Pattern that matches one or more digit characters.
    */
  val digits: Pattern = rep1(digit)

  /** Pattern that matches any whitespace character.
    */
  val whitespace: Pattern = clas(_.isWhitespace)

  /** Pattern that matches zero or more whitespace characters.
    */
  val ws: Pattern = rep(whitespace)

  /** Pattern that matches one or more whitespace characters.
    */
  val ws1: Pattern = rep1(whitespace)

  /** Pattern that matches an identifier: a letter or underscore followed by zero or more letters, digits, or
    * underscores, followed by whitespace.
    */
  val ident: Pattern = string((alpha | '_') ~ rep(alphanum | '_')) ~ ws

  /** Pattern that matches a numeric literal in various formats:
    *   - Integer: one or more digits
    *   - Decimal: digits with decimal point
    *   - Scientific notation: decimal or integer with optional exponent
    */
  val number: Pattern =
    (rep(digit) ~ '.' ~ digits | digits ~ '.') ~
      opt((elem('e') | 'E') ~ opt(elem('+') | '-') ~ digits) |
      digits

  /** Captures the text matched by a pattern as a string.
    *
    * @param p
    *   the pattern to match
    * @return
    *   a pattern that captures the matched input as a string
    */
  def string(p: Pattern): Pattern = capture(p)(_.listElem(_).mkString)

}
