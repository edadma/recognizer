package io.github.edadma.recognizer

import scala.collection.mutable

trait Testing extends Recognizer[Char] {

  var pattern: Pattern = _

  class StringInput(s: String, idx: Int = 0) extends Input[Char] {
    def eoi: Boolean = idx >= s.length

    def elem: Char = s(idx)

    def next: StringInput = new StringInput(s, idx + 1)

    def rest: List[Char] = s.substring(idx).toList

    override def toString: String = s"StringInput<$rest>"
  }

  def parse(s: String, p: Pattern): Option[(Option[Any], String, mutable.Stack[Choice])] = {
    pattern = p
    run(new StringInput(s), pattern) map {
      case (v, r, c) => (v, r.rest.mkString, c)
    }
  }

}
