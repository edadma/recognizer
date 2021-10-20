package io.github.edadma.recognizer

case class StringInput(s: String, idx: Int = 0) extends Input[Char, Char] {
  def eoi: Boolean = idx >= s.length

  def elem: Char = s(idx)

  def wrapped: Char = s(idx)

  def next: StringInput = StringInput(s, idx + 1)
}
