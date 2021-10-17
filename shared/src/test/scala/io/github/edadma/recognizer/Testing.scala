package io.github.edadma.recognizer

trait Testing {

  class StringInput(s: String, idx: Int = 0) extends Input[Char] {
    def eoi: Boolean = idx >= s.length

    def elem: Char = s(idx)

    def next: StringInput = new StringInput(s, idx + 1)

    override def toString: String = s"StringInput<${s.substring(idx)}>"
  }

}
