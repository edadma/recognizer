package io.github.edadma.recognizer

trait Input[E] {
  def eoi: Boolean
  def elem: E
  def next: Input[E]
}
