package io.github.edadma.recognizer

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.{implicitConversions, postfixOps}

trait Recognizer[W, E] {

  type I = Input[W, E]

  implicit def elem(e: E): Pattern = Clas(_ == e)

  def seq(es: E*): Pattern = Match(es)

  def clas(c: E => Boolean): Pattern = Clas(c)

  def anyOf(es: E*): Pattern = clas(es contains _)

  def noneOf(es: E*): Pattern = clas(e => !(es contains e))

  def any: Pattern = clas(_ => true)

  def nop: Pattern = Nop

  def failed: Pattern = Fail

  def !! : Pattern = Cut

  def fence: Pattern = Fence

  def not(p: Pattern): Pattern = fence ~ (p ~ !! ~ failed | nop)

  def opt(p: Pattern): Pattern = p | nop

  def optt(p: Pattern, arity: Int)(f: Seq[Any] => Any): Pattern =
    p ~ transform(arity)(args => Some(f(args))) | push(None)

  def opta[A](p: Pattern)(f: A => Any): Pattern = p ~ action[A](a => Some(f(a))) | push(None)

  def opti[A](p: Pattern): Pattern = opta[Any](p)(identity)

  def optr(p: Pattern): Pattern = nop | p

  def optrt(p: Pattern, arity: Int)(f: Seq[Any] => Any): Pattern =
    push(None) | p ~ transform(arity)(args => Some(f(args)))

  def optra[A](p: Pattern)(f: A => Any): Pattern = push(None) | p ~ action[A](a => Some(f(a)))

  def optri(p: Pattern): Pattern = optra[Any](p)(identity)

  def nonStrict(p: => Pattern): Pattern = NonStrict(() => p)

  def testValues(c: List[Any] => Boolean): Pattern = Test(c)

  def test[A](c: A => Boolean): Pattern = Test(values => c(values.head.asInstanceOf[A]))

  def rep1(p: Pattern): Pattern = {
    lazy val pat: Pattern = p ~ opt(nonStrict(pat))

    pat
  }

  def repr1(p: Pattern): Pattern = {
    lazy val pat: Pattern = p ~ optr(nonStrict(pat))

    pat
  }

  def rep(p: Pattern): Pattern = opt(rep1(p))

  def repr(p: Pattern): Pattern = optr(repr1(p))

  def rep1t(p: Pattern, arity: Int)(f: Seq[Any] => Any): Pattern =
    push(new ListBuffer[Any]) ~ rep1(p ~ transform(arity)(f) ~ action2[ListBuffer[Any], Any] { (list, item) =>
      list += item
      list
    }) ~ action[ListBuffer[Any]](_.toList)

  def rep1a[A](p: Pattern)(f: A => Any): Pattern =
    push(new ListBuffer[Any]) ~ rep1(p ~ action(f) ~ action2[ListBuffer[Any], Any] { (list, item) =>
      list += item
      list
    }) ~ action[ListBuffer[Any]](_.toList)

  def rep1i(p: Pattern): Pattern = rep1a[Any](p)(identity)

  def repr1t(p: Pattern, arity: Int)(f: Seq[Any] => Any): Pattern =
    push(new ListBuffer[Any]) ~ repr1(p ~ transform(arity)(f) ~ action2[ListBuffer[Any], Any] { (list, item) =>
      list += item
      list
    }) ~ action[ListBuffer[Any]](_.toList)

  def repr1a[A](p: Pattern)(f: A => Any): Pattern =
    push(new ListBuffer[Any]) ~ repr1(p ~ action(f) ~ action2[ListBuffer[Any], Any] { (list, item) =>
      list += item
      list
    }) ~ action[ListBuffer[Any]](_.toList)

  def repr1i(p: Pattern): Pattern = repr1a[Any](p)(identity)

  def rept(p: Pattern, arity: Int)(f: Seq[Any] => Any): Pattern =
    push(new ListBuffer[Any]) ~ rep(p ~ transform(arity)(f) ~ action2[ListBuffer[Any], Any] { (list, item) =>
      list += item
      list
    }) ~ action[ListBuffer[Any]](_.toList)

  def repa[A](p: Pattern)(f: A => Any): Pattern =
    push(new ListBuffer[Any]) ~ rep(p ~ action(f) ~ action2[ListBuffer[Any], Any] { (list, item) =>
      list += item
      list
    }) ~ action[ListBuffer[Any]](_.toList)

  def repi(p: Pattern): Pattern = repa[Any](p)(identity)

  def reprt(p: Pattern, arity: Int)(f: Seq[Any] => Any): Pattern =
    push(new ListBuffer[Any]) ~ repr(p ~ transform(arity)(f) ~ action2[ListBuffer[Any], Any] { (list, item) =>
      list += item
      list
    }) ~ action[ListBuffer[Any]](_.toList)

  def repra[A](p: Pattern)(f: A => Any): Pattern =
    push(new ListBuffer[Any]) ~ repr(p ~ action(f) ~ action2[ListBuffer[Any], Any] { (list, item) =>
      list += item
      list
    }) ~ action[ListBuffer[Any]](_.toList)

  def push(v: Any): Pattern = Push(v)

  def pointer: Pattern = Pointer

  def capture(p: Pattern)(action: (I, I) => Any): Pattern = pointer ~ p ~ pointer ~ action2[I, I](action)

  def captureWrapped(p: Pattern): Pattern = capture(p)(_.listWrapped(_))

  def transform(arity: Int)(f: Seq[Any] => Any): Pattern = Transform(arity, f)

  def action[A](f: A => Any): Pattern = transform(1) { case Seq(a) => f(a.asInstanceOf[A]) }

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

  protected case object Fence extends Pattern with Choice
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
  protected case class Test(p: List[Any] => Boolean) extends Pattern

  protected trait Choice
  protected case class ChoicePoint(input: I, pattern: Pattern, call: List[Pattern], value: List[Any]) extends Choice

  var runlimit: Int = Int.MaxValue

  private[recognizer] def debug(s: String): Unit =
    if (runlimit < Int.MaxValue)
      println(s)

  class Runstate private[recognizer] (private[recognizer] var pointer: I, pat: Pattern) {
    private[recognizer] val choice = new mutable.Stack[Choice]
    private[recognizer] var call: List[Pattern] = Nil
    private[recognizer] var value: List[Any] = Nil
    private[recognizer] var ip: Pattern = pat

    def values: List[Any] = value

    private[recognizer] def push(p: Pattern): Unit = call = p :: call

    private[recognizer] def advance(): Unit =
      ip = call match {
        case h :: t =>
          call = t
          h
        case Nil => null
      }

    def backtrack: Boolean = {
      debug(s"backtrack $choice")
      if (choice.nonEmpty) {
        choice.pop() match {
          case ChoicePoint(p, n, c, v) =>
            pointer = p
            call = c
            value = v
            ip = n
            true
          case Fence => backtrack
        }
      } else false
    }
  }

  def runAll(input: I, pat: Pattern): List[(Option[Any], I)] =
    run(input, pat) match {
      case None => Nil
      case Some(r) =>
        val buf = new ListBuffer[(Option[Any], I)]

        result(r)

        @tailrec
        def result(r: (Option[Any], I, Runstate)): Unit =
          r match {
            case (v, u, s) =>
              buf += ((v, u))
              rerun(s) match {
                case Some(r) => result(r)
                case None    =>
              }
          }

        buf.toList
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

      debug(s"run ${state.call} ${state.pointer}")

      state.ip match {
        case null => true
        case Cut =>
          debug(s"cut")
          while (state.choice.top.isInstanceOf[ChoicePoint]) state.choice.pop()

          if (state.choice.nonEmpty) {
            state.choice.pop()
            state.advance()
            run
          } else sys.error("fence not encountered during cut")
        case Fence =>
          state.choice push Fence
          state.advance()
          run
        case Alternative(p, q) =>
          debug(s"alternative $p  $q")
          state.choice push ChoicePoint(state.pointer, q, state.call, state.value)
          state.ip = p
          run
        case Match(s) =>
          debug(s"match $s")
          val it = s.iterator

          while (it.hasNext && !state.pointer.eoi && state.pointer.elem == it.next()) {
            state.pointer = state.pointer.next
          }

          if (!it.hasNext) {
            state.advance()
            run
          } else if (state.backtrack) run
          else false
        case Clas(c) =>
          if (!state.pointer.eoi && c(state.pointer.elem)) {
            state.pointer = state.pointer.next
            state.advance()
            run
          } else if (state.backtrack) run
          else false
        case Push(v) =>
          state.value = v :: state.value
          state.advance()
          run
        case Transform(arity, f) =>
          debug(s"transform before ${state.value}")

          val (args, rest) = state.value splitAt arity

          state.value = f(args.reverse) :: rest
          debug(s"          after  ${state.value}")
          state.advance()
          run
        case Sequence(p, q) =>
          debug(s"sequence $p  $q")
          state.ip = p
          state.push(q)
          run
        case Nop =>
          state.advance()
          run
        case Fail =>
          debug(s"fail")
          if (state.backtrack) run
          else false
        case Pointer =>
          state.value = state.pointer :: state.value
          state.advance()
          run
        case NonStrict(p) =>
          state.ip = p()
          run
        case Test(c) =>
          if (c(state.value)) {
            state.advance()
            run
          } else if (state.backtrack) run
          else false
      }
    }

    if (run) Some((state.value.headOption, state.pointer, state))
    else None
  }

}
