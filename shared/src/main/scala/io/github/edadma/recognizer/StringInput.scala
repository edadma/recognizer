package io.github.edadma.recognizer

case class StringInput(s: String, idx: Int = 0) extends Input[Char] {
  def eoi: Boolean = idx >= s.length

  def elem: Char = s(idx)

  def next: StringInput = new StringInput(s, idx + 1)

  def rest: List[Char] = s.substring(idx).toList

  override def toString: String = s"StringInput<${rest.mkString(", ")}>"
}
