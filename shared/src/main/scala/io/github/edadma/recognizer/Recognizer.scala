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

  protected case object Nop extends Pattern
  protected case object Fail extends Pattern
  protected case object Pointer extends Pattern
  protected case class Sequence(p: Pattern, q: Pattern) extends Pattern
  protected case class Alternative(p: Pattern, q: Pattern) extends Pattern
  protected case class Clas(c: E => Boolean) extends Pattern
  protected case class Match(e: Seq[E]) extends Pattern
  protected case class Push(v: Any) extends Pattern
  protected case class Transform(arity: Int, f: Seq[Any] => Any) extends Pattern
  protected case class NonStrict(p: () => Pattern) extends Pattern

  protected case class Choice(input: I, pattern: Pattern, call: List[Pattern], value: List[Any])

  var runlimit: Int = Int.MaxValue

  def run(input: I, pat: Pattern): Option[(Option[Any], I)] = {
    var call: List[Pattern] = Nil
    val choice = new mutable.Stack[Choice]
    var value: List[Any] = Nil
    var pointer: I = input

    def debug(s: String): Unit =
      if (runlimit < Int.MaxValue)
        println(s)

    def push(p: Pattern): Unit = call = p :: call

    def pop: Pattern =
      call match {
        case h :: t =>
          call = t
          h
        case _ => sys.error("no more patterns")
      }

    push(pat)

    def backtrack: Boolean = {
      debug(s"backtrack $choice")
      if (choice.nonEmpty) {
        val Choice(p, n, c, v) = choice.pop()

        pointer = p
        call = n :: c
        value = v
        true
      } else false
    }

    @tailrec
    def run: Boolean = {
      if (runlimit < Int.MaxValue) {
        runlimit -= 1

        if (runlimit < 0) {
          println("LIMIT")
          return false
        }
      }

      if (call.nonEmpty) {
        debug(s"run $call $pointer")
        pop match {
          case Alternative(p, q) =>
            choice push Choice(pointer, q, call, value)
            push(p)
            run
          case Match(s) =>
            val it = s.iterator

            while (it.hasNext && !pointer.eoi && pointer.elem == it.next()) {
              pointer = pointer.next
            }

            if (!it.hasNext) run
            else if (backtrack) run
            else false
          case Clas(c) =>
            if (!pointer.eoi && c(pointer.elem)) {
              pointer = pointer.next
              run
            } else if (backtrack) run
            else false
          case Push(v) =>
            value = v :: value
            run
          case Transform(arity, f) =>
            debug(s"transform before $value")

            val (args, rest) = value splitAt arity

            value = f(args.reverse) :: rest
            debug(s"          after  $value")
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
            value = pointer :: value
            run
          case NonStrict(p) =>
            push(p())
            run
        }
      } else
        true
    }

    if (run) Some((value.headOption, pointer))
    else None
  }

}
