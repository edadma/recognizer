package io.github.edadma.recognizer

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.{implicitConversions, postfixOps}

trait Recognizer[E] {

  type I = Input[E]

  implicit def elem(e: E): Pattern = Clas(_ == e)

  def seq(es: E*): Pattern = Match(es)

  def clas(c: E => Boolean): Pattern = Clas(c)

  def anyOf(es: E*): Pattern = clas(es contains _)

  def noneOf(es: E*): Pattern = clas(e => !(es contains e))

  def any: Pattern = clas(_ => true)

  def nop: Pattern = Nop

  def fail: Pattern = Fail

  def >! : Pattern = Cut

  def !< : Pattern = Fence

  def not(p: Pattern): Pattern = !< ~ p ~ >! ~ fail | nop

  def opt(p: Pattern): Pattern = p | nop

  def opt(p: Pattern, arity: Int)(f: Seq[Any] => Any): Pattern =
    p ~ transform(arity)(args => Some(f(args))) | push(None)

  def rep1(p: Pattern): Pattern = {
    lazy val pat: Pattern = p ~ opt(NonStrict(() => pat))

    pat
  }

  def rep(p: Pattern): Pattern = opt(rep1(p))

  def rep1(p: Pattern, arity: Int)(f: Seq[Any] => Any): Pattern =
    push(new ListBuffer[Any]) ~ rep1(p ~ transform(arity)(f) ~ transform(2) {
      case Seq(list: ListBuffer[_], item) =>
        list.asInstanceOf[ListBuffer[Any]] += item
        list
    }) ~ transform(_.asInstanceOf[Seq[ListBuffer[Any]]].head.toList)

  def rep(p: Pattern, arity: Int)(f: Seq[Any] => Any): Pattern =
    push(new ListBuffer[Any]) ~ rep(p ~ transform(arity)(f) ~ transform(2) {
      case Seq(list: ListBuffer[_], item) =>
        list.asInstanceOf[ListBuffer[Any]] += item
        list
    }) ~ transform(_.asInstanceOf[Seq[ListBuffer[Any]]].head.toList)

  def push(v: Any): Pattern = Push(v)

  def pointer: Pattern = Pointer

  def capture(p: Pattern): Pattern =
    pointer ~ p ~ pointer ~ transform(2) {
      case Seq(start, end) => start.asInstanceOf[I].list(end.asInstanceOf[I]).get
    }

  def transform(arity: Int)(f: Seq[Any] => Any): Pattern = Transform(arity, f)

  def transform(f: Seq[Any] => Any): Pattern = Transform(1, f)

  def action2[A, B](f: (A, B) => Any): Pattern = transform(2) {
    case Seq(a, b) => f(a.asInstanceOf[A], b.asInstanceOf[B])
  }

  def action3[A, B, C](f: (A, B, C) => Any): Pattern = transform(3) {
    case Seq(a, b, c) => f(a.asInstanceOf[A], b.asInstanceOf[B], c.asInstanceOf[C])
  }

  def action4[A, B, C, D](f: (A, B, C, D) => Any): Pattern = transform(4) {
    case Seq(a, b, c, d) => f(a.asInstanceOf[A], b.asInstanceOf[B], c.asInstanceOf[C], d.asInstanceOf[D])
  }

  trait Pattern {
    def ~(that: Pattern): Pattern = Sequence(this, that)
    def |(that: Pattern): Pattern = Alternative(this, that)
  }

  protected case object Fence extends Pattern
  protected case object Cut extends Pattern
  protected case object Nop extends Pattern
  protected case object Fail extends Pattern
  protected case object Pointer extends Pattern
  protected case class Sequence(p: Pattern, q: Pattern) extends Pattern
  protected case class Alternative(p: Pattern, q: Pattern) extends Pattern
  protected case class Not(p: Pattern) extends Pattern
  protected case class Clas(c: E => Boolean) extends Pattern
  protected case class Match(e: Seq[E]) extends Pattern
  protected case class Push(v: Any) extends Pattern
  protected case class Transform(arity: Int, f: Seq[Any] => Any) extends Pattern
  protected case class NonStrict(p: () => Pattern) extends Pattern

  protected trait Choice
  protected case class ChoicePoint(input: I, pattern: Pattern, call: List[Pattern], value: List[Any]) extends Choice
  protected case object Ceiling extends Choice

  var runlimit: Int = Int.MaxValue

  private[recognizer] def debug(s: String): Unit =
    if (runlimit < Int.MaxValue)
      println(s)

  class Runstate private[recognizer] (private[recognizer] var pointer: I, pat: Pattern) {
    private[recognizer] val choice = new mutable.Stack[Choice]
    private[recognizer] var call: List[Pattern] = Nil
    private[recognizer] var value: List[Any] = Nil

    push(pat)

    def values: List[Any] = value

    private[recognizer] def push(p: Pattern): Unit = call = p :: call

    private[recognizer] def pop: Pattern =
      call match {
        case h :: t =>
          call = t
          h
        case _ => sys.error("no more patterns")
      }

    def backtrack: Boolean = {
      debug(s"backtrack $choice")
      if (choice.nonEmpty) {
        choice.pop() match {
          case ChoicePoint(p, n, c, v) =>
            pointer = p
            call = n :: c
            value = v
            true
          case Ceiling => backtrack
        }
      } else false
    }
  }

  def run(input: I, pat: Pattern): Option[(Option[Any], I, Runstate)] = run(new Runstate(input, pat))

  def rerun(state: Runstate): Option[(Option[Any], I, Runstate)] =
    if (state.backtrack) run(state)
    else None

  def run(state: Runstate): Option[(Option[Any], I, Runstate)] = {
    @tailrec
    def run: Boolean = {
      if (runlimit < Int.MaxValue) {
        runlimit -= 1

        if (runlimit < 0) {
          println("LIMIT")
          return false
        }
      }

      if (state.call.nonEmpty) {
        debug(s"run ${state.call} ${state.pointer}")
        state.pop match {
          case Cut =>
            debug(s"cut")
            while (state.choice.top.isInstanceOf[ChoicePoint]) state.choice.pop()

            if (state.choice.nonEmpty) {
              state.choice.pop()
              run
            } else sys.error("ceiling not encountered during cut")
          case Fence =>
            state.choice push Ceiling
            run
          case Alternative(p, q) =>
            debug(s"alternative $p  $q")
            state.choice push ChoicePoint(state.pointer, q, state.call, state.value)
            state.push(p)
            run
          case Match(s) =>
            debug(s"match $s")
            val it = s.iterator

            while (it.hasNext && !state.pointer.eoi && state.pointer.elem == it.next()) {
              state.pointer = state.pointer.next
            }

            if (!it.hasNext) run
            else if (state.backtrack) run
            else false
          case Clas(c) =>
            if (!state.pointer.eoi && c(state.pointer.elem)) {
              state.pointer = state.pointer.next
              run
            } else if (state.backtrack) run
            else false
          case Push(v) =>
            state.value = v :: state.value
            run
          case Transform(arity, f) =>
            debug(s"transform before ${state.value}")

            val (args, rest) = state.value splitAt arity

            state.value = f(args.reverse) :: rest
            debug(s"          after  ${state.value}")
            run
          case Sequence(p, q) =>
            debug(s"sequence $p  $q")
            state.push(q)
            state.push(p)
            run
          case Nop => run
          case Fail =>
            debug(s"fail")
            if (state.backtrack) run
            else false
          case Pointer =>
            state.value = state.pointer :: state.value
            run
          case NonStrict(p) =>
            state.push(p())
            run
        }
      } else
        true
    }

    if (run) Some((state.value.headOption, state.pointer, state))
    else None
  }

}
