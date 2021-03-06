package io.github.edadma.recognizer

import scala.collection.mutable.ListBuffer

trait Input[W, E] {
  def eoi: Boolean
  def elem: E
  def wrapped: W
  def next: Input[W, E]

  def rest: List[E] = {
    val buf = new ListBuffer[E]
    var e = this

    while (!e.eoi) {
      buf += e.elem
      e = e.next
    }

    buf.toList
  }

  def listElem(end: Input[W, E]): List[E] = {
    val buf = new ListBuffer[E]
    var e = this

    while (!e.eoi && e != end) {
      buf += e.elem
      e = e.next
    }

    if (e.eoi && !end.eoi) sys.error(s"listElem: hit eoi unexpectedly: end: $end")
    else buf.toList
  }

  def listWrapped(end: Input[W, E]): List[W] = {
    val buf = new ListBuffer[W]
    var e = this

    while (!e.eoi && e != end) {
      buf += e.wrapped
      e = e.next
    }

    if (e.eoi && !end.eoi) sys.error(s"listElem: hit eoi unexpectedly: end: $end")
    else buf.toList
  }

  def equals(obj: Any): Boolean

  override def toString: String = s"<${rest take 10 mkString ", "}>"
}
