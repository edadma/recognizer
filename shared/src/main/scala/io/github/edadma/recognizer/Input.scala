package io.github.edadma.recognizer

import scala.collection.mutable.ListBuffer

trait Input[E] {
  def eoi: Boolean
  def elem: E
  def next: Input[E]
  def rest: List[E]

  def list(end: Input[E]): Option[List[E]] = {
    val buf = new ListBuffer[E]
    var e = this

    while (!e.eoi && e != end) {
      buf += e.elem
      e = e.next
    }

    if (e.eoi && !end.eoi) None
    else Some(buf.toList)
  }
}
