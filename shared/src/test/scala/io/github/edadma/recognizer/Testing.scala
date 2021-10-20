package io.github.edadma.recognizer

trait Testing extends CharRecognizer[Char] {

  def parse(s: String, p: Pattern): Option[(Option[Any], String)] = {
    run(StringInput(s), p) map {
      case (v, r, _) => (v, r.rest.mkString)
    }
  }

}
