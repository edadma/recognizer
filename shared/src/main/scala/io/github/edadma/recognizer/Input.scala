package io.github.edadma.recognizer

abstract class Input[E] {
  def eoi: Boolean
  def elem: E
  def next: Input[E]
}
