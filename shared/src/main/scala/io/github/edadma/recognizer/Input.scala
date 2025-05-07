package io.github.edadma.recognizer

import scala.collection.mutable.ListBuffer

/** Represents a position in an input stream with elements of type E and wrapped values of type W.
  *
  * The Input trait provides methods to access the current element, check for end of input, advance to the next
  * position, and collect elements or wrapped values between positions.
  *
  * @tparam W
  *   the type of wrapped values associated with elements
  * @tparam E
  *   the type of elements in the input stream
  */
trait Input[W, E] {

  /** Indicates whether the current position is at the end of input.
    *
    * @return
    *   true if at end of input, false otherwise
    */
  def eoi: Boolean

  /** Returns the element at the current position.
    *
    * @return
    *   the current element
    */
  def elem: E

  /** Returns the wrapped value associated with the current element.
    *
    * @return
    *   the wrapped value for the current element
    */
  def wrapped: W

  /** Returns the previous Input representing the position before the current one.
    *
    * @return
    *   the previous position in the input stream
    */
  def prev: Option[Input[W, E]]

  /** Returns a new Input representing the position after the current one.
    *
    * @return
    *   the next position in the input stream
    */
  def next: Input[W, E]

  /** Collects all elements from the current position to the end of input.
    *
    * @return
    *   a list of all remaining elements
    */
  def rest: List[E] = {
    val buf = new ListBuffer[E]
    var e   = this

    while (!e.eoi) {
      buf += e.elem
      e = e.next
    }

    buf.toList
  }

  /** Collects all elements between the current position and a specified end position.
    *
    * @param end
    *   the ending position (exclusive)
    * @return
    *   a list of elements between current position and end
    * @throws RuntimeException
    *   if end of input is reached before the end position
    */
  def listElem(end: Input[W, E]): List[E] = {
    val buf = new ListBuffer[E]
    var e   = this

    while (!e.eoi && e != end) {
      buf += e.elem
      e = e.next
    }

    if (e.eoi && !end.eoi) sys.error(s"listElem: hit eoi unexpectedly: end: $end")
    else buf.toList
  }

  /** Collects all wrapped values between the current position and a specified end position.
    *
    * @param end
    *   the ending position (exclusive)
    * @return
    *   a list of wrapped values between current position and end
    * @throws RuntimeException
    *   if end of input is reached before the end position
    */
  def listWrapped(end: Input[W, E]): List[W] = {
    val buf = new ListBuffer[W]
    var e   = this

    while (!e.eoi && e != end) {
      buf += e.wrapped
      e = e.next
    }

    if (e.eoi && !end.eoi) sys.error(s"listElem: hit eoi unexpectedly: end: $end")
    else buf.toList
  }

  /** Tests equality between Input positions.
    *
    * @param obj
    *   the object to compare with
    * @return
    *   true if the positions are equal, false otherwise
    */
  def equals(obj: Any): Boolean

  /** Returns a string representation of this input position.
    *
    * @return
    *   a string representing the next few elements from this position
    */
  override def toString: String = s"<${rest take 10 mkString ", "}>"
}
