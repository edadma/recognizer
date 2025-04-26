package io.github.edadma.recognizer

/** An implementation of the Input trait for string inputs.
  *
  * StringInput provides a character-based input implementation where each position represents a character in a string.
  * The wrapped value is the same as the element (the character itself).
  *
  * @param s
  *   the string to process
  * @param idx
  *   the current position in the string (0-based index)
  */
case class StringInput(s: String, idx: Int = 0) extends Input[Char, Char] {

  /** Checks if the current position is at the end of input.
    *
    * @return
    *   true if the position is beyond the end of the string
    */
  def eoi: Boolean = idx >= s.length

  /** Returns the character at the current position.
    *
    * @return
    *   the current character
    */
  def elem: Char = s(idx)

  /** Returns the wrapped value, which is the same as the character for StringInput.
    *
    * @return
    *   the current character
    */
  def wrapped: Char = s(idx)

  /** Returns a new StringInput at the next position.
    *
    * @return
    *   a new StringInput with index incremented by 1
    */
  def next: StringInput = StringInput(s, idx + 1)
}
