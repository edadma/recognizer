package io.github.edadma.recognizer

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.{implicitConversions, postfixOps}

trait Recognizer[E] {

  implicit def elem(e: E): Pattern = Elem(e)

  def nop: Pattern = Nop

  def fail: Pattern = Fail

  def opt(p: Pattern): Pattern = p | nop

  def opt(p: Pattern, arity: Int)(f: Seq[Any] => Any): Pattern =
    p ~ transform(arity)(args => Some(f(args))) | nop ~ push(None)

  def push(v: Any): Pattern = Push(v)

  def pointer: Pattern = Pointer

//  def capture(p: Pattern): Pattern =
//    pointer ~ p ~ pointer ~ (transform(2) {
//      case Seq(start, end) =>
//    })

  def transform(arity: Int)(f: Seq[Any] => Any): Pattern = Transform(arity, f)

  trait Pattern {
    def ~(that: Pattern): Pattern = Sequence(this, that)
    def |(that: Pattern): Pattern = Alternative(this, that)
  }

  private case object Nop extends Pattern
  private case object Fail extends Pattern
  private case object Pointer extends Pattern
  private case class Sequence(p: Pattern, q: Pattern) extends Pattern
  private case class Alternative(p: Pattern, q: Pattern) extends Pattern
  private case class Elem(e: E) extends Pattern
  private case class Push(v: Any) extends Pattern
  private case class Transform(arity: Int, f: Seq[Any] => Any) extends Pattern

  def run[I <: Input[E]](input: I, pat: Pattern): Option[(Option[Any], I)] = {
    case class Choice(input: I, pattern: Pattern, call: List[Pattern])
    var call: List[Pattern] = Nil
    val choice = new mutable.Stack[Choice]
    val value = new mutable.Stack[Any]
    var pointer: I = input
    var limit = Int.MaxValue

    def push(p: Pattern): Unit = call = p :: call

    def pop: Pattern =
      call match {
        case h :: t =>
          call = t
          h
        case _ => sys.error("no more patterns")
      }

    push(pat)

    def backtrack: Boolean =
      choice headOption match {
        case None => false
        case Some(Choice(p, n, c)) =>
          pointer = p
          call = n :: c
          true
      }

    @tailrec
    def run: Boolean = {
      if (limit < Int.MaxValue) {
        limit -= 1

        if (limit < 0) {
          println("LIMIT")
          return false
        }
      }

      if (call.nonEmpty) {
        pop match {
          case Alternative(p, q) =>
            choice push Choice(pointer, q, call)
            push(p)
            run
          case Elem(e) =>
            if (!pointer.eoi && pointer.elem == e) {
              pointer = pointer.next.asInstanceOf[I]
              run
            } else if (backtrack) run
            else false
          case Push(v) =>
            value push v
            run
          case Transform(arity, f) =>
            value push f(value.take(arity).reverse.toList)
            run
          case Sequence(p, q) =>
            push(q)
            push(p)
            run
          case Nop => run
          case Fail =>
            if (backtrack) run
            else false
          case Pointer =>
            value push pointer
            run
        }
      } else
        true
    }

    if (run) Some((value.headOption, pointer))
    else None
  }

}
