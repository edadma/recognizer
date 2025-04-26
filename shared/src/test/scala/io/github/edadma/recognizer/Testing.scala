package io.github.edadma.recognizer

/** A utility trait that facilitates easy testing of character patterns.
  *
  * The Testing trait mixes in CharRecognizer and provides a convenient parse method that works directly with strings.
  * This makes it ideal for quick tests and REPL usage.
  */
trait Testing extends CharRecognizer[Char]:
  /** Parses a string against a pattern and returns the result.
    *
    * This method applies a pattern to a string and, if successful, returns the matched value and the remaining unparsed
    * portion of the string.
    *
    * @param s
    *   the string to parse
    * @param p
    *   the pattern to apply
    * @return
    *   Some((value, remaining string)) if successful, None if parsing failed
    */
  def parse(s: String, p: Pattern): Option[(Option[Any], String)] = {
    run(StringInput(s), p) map { case (v, r, _) =>
      (v, r.rest.mkString)
    }
  }
