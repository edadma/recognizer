package io.github.edadma.recognizer

import scala.collection.mutable

trait Testing extends CharRecognizer {

  var pattern: Pattern = _

  def parse(s: String, p: Pattern): Option[(Option[Any], String)] = {
    pattern = p
    run(new StringInput(s), pattern) map {
      case (v, r) => (v, r.rest.mkString)
    }
  }

}
