package io.github.edadma.recognizer

object IntExample extends TestingInt {
  @main def run(): Unit = {
    // say we want “one or more 1’s, then a 2”
    val pattern: Pattern = rep1(elem(1)) ~ elem(2)

    val input = List(1, 1, 1, 2, 3, 1)
    parse(input, pattern) match {
      case Some((v, leftover)) =>
        println(s"Matched! value stack = $v, leftover = $leftover")
      case None =>
        println("No match")
    }
  }
}

case class IntListInput(xs: List[Int], idx: Int = 0, prev: Option[IntListInput] = None) extends Input[Int, Int] {
  def eoi: Boolean       = idx >= xs.length
  def elem: Int          = xs(idx)
  def wrapped: Int       = xs(idx)
  def next: IntListInput = copy(idx = idx + 1, prev = Some(this))
  override def equals(obj: Any) = obj match {
    case IntListInput(ys, j, _) => xs == ys && idx == j
    case _                      => false
  }
}

// 2) A small helper to parse Int lists:
trait TestingInt extends Recognizer[Int, Int] {
  def parse(xs: List[Int], p: Pattern): Option[(Option[Any], List[Int])] =
    run(IntListInput(xs), p).map { case (v, rem, _) =>
      (v, rem.rest)
    }
}
