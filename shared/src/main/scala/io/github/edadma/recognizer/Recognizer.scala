package io.github.edadma.recognizer

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.{implicitConversions, postfixOps}

/** Core trait providing pattern matching functionality over an input stream.
  *
  * The Recognizer trait defines a composable pattern matching DSL with support for backtracking, value capture, and
  * transformation. It implements a recursive descent parser with explicit backtracking control through choice points.
  *
  * @tparam W
  *   the type of wrapped values associated with input elements
  * @tparam E
  *   the type of elements in the input stream
  */
trait Recognizer[W, E] {

  /** Type alias for the input type used by this recognizer */
  type I = Input[W, E]

  /** Implicitly converts an element to a pattern that matches that element.
    *
    * @param e
    *   the element to match
    * @return
    *   a pattern that matches the given element
    */
  implicit def elem(e: E): Pattern = Clas(_ == e)

  /** Creates a pattern that matches a sequence of elements.
    *
    * @param es
    *   the elements to match in sequence
    * @return
    *   a pattern matching the given sequence of elements
    */
  def seq(es: E*): Pattern = Match(es)

  /** Creates a pattern that matches any element satisfying a predicate.
    *
    * @param c
    *   the predicate function
    * @return
    *   a pattern that matches elements satisfying the predicate
    */
  def clas(c: E => Boolean): Pattern = Clas(c)

  /** Creates a pattern that matches any element in a specified set.
    *
    * @param es
    *   the elements to match against
    * @return
    *   a pattern that matches any element in the set
    */
  def anyOf(es: E*): Pattern = clas(es contains _)

  /** Creates a pattern that matches any element not in a specified set.
    *
    * @param es
    *   the elements to exclude
    * @return
    *   a pattern that matches any element not in the set
    */
  def noneOf(es: E*): Pattern = clas(e => !(es contains e))

  /** Creates a pattern that matches any element.
    *
    * @return
    *   a pattern that matches any element
    */
  def any: Pattern = clas(_ => true)

  /** A pattern that always succeeds without consuming input.
    *
    * @return
    *   a pattern that always succeeds
    */
  def nop: Pattern = Nop

  /** A pattern that always fails.
    *
    * @return
    *   a pattern that always fails
    */
  def failed: Pattern = Fail

  /** Cut operator - disallows backtracking past this point.
    *
    * @return
    *   a pattern representing a cut point
    */
  def !! : Pattern = Cut

  /** Creates a fence marker for backtracking control.
    *
    * @return
    *   a pattern representing a fence
    */
  def fence: Pattern = Fence

  /** Creates a negative lookahead pattern that succeeds only if the given pattern fails.
    *
    * @param p
    *   the pattern to negate
    * @return
    *   a pattern that succeeds only if p fails
    */
  def not(p: Pattern): Pattern = fence ~ (p ~ !! ~ failed | nop)

  /** Creates an optional pattern that succeeds even if the given pattern fails.
    *
    * @param p
    *   the pattern that is optional
    * @return
    *   a pattern that succeeds whether p matches or not
    */
  def opt(p: Pattern): Pattern = p | nop

  /** Creates an optional pattern with transformation that applies a function to matched values if the pattern succeeds,
    * or pushes None if it fails.
    *
    * @param p
    *   the pattern that is optional
    * @param arity
    *   the number of values to transform
    * @param f
    *   the transformation function
    * @return
    *   a pattern that pushes Some(result) or None
    */
  def optt(p: Pattern, arity: Int)(f: Seq[Any] => Any): Pattern =
    p ~ transform(arity)(args => Some(f(args))) | push(None)

  /** Creates an optional pattern with action that applies a function to the matched value if the pattern succeeds, or
    * pushes None if it fails.
    *
    * @param p
    *   the pattern that is optional
    * @param f
    *   the action function
    * @return
    *   a pattern that pushes Some(result) or None
    */
  def opta[A](p: Pattern)(f: A => Any): Pattern = p ~ action[A](a => Some(f(a))) | push(None)

  /** Creates an optional pattern that preserves the matched value if the pattern succeeds, or pushes None if it fails.
    *
    * @param p
    *   the pattern that is optional
    * @return
    *   a pattern that pushes Some(value) or None
    */
  def opti[A](p: Pattern): Pattern = opta[Any](p)(identity)

  /** Creates a right-associative optional pattern.
    *
    * This is like opt() but with reversed order of alternatives, which can affect the order of backtracking.
    *
    * @param p
    *   the pattern that is optional
    * @return
    *   a right-associative optional pattern
    */
  def optr(p: Pattern): Pattern = nop | p

  /** Creates a right-associative optional pattern with transformation.
    *
    * @param p
    *   the pattern that is optional
    * @param arity
    *   the number of values to transform
    * @param f
    *   the transformation function
    * @return
    *   a pattern that pushes None or Some(result)
    */
  def optrt(p: Pattern, arity: Int)(f: Seq[Any] => Any): Pattern =
    push(None) | p ~ transform(arity)(args => Some(f(args)))

  /** Creates a right-associative optional pattern with action.
    *
    * @param p
    *   the pattern that is optional
    * @param f
    *   the action function
    * @return
    *   a pattern that pushes None or Some(result)
    */
  def optra[A](p: Pattern)(f: A => Any): Pattern = push(None) | p ~ action[A](a => Some(f(a)))

  /** Creates a right-associative optional pattern that preserves the matched value.
    *
    * @param p
    *   the pattern that is optional
    * @return
    *   a pattern that pushes None or Some(value)
    */
  def optri(p: Pattern): Pattern = optra[Any](p)(identity)

  /** Creates a pattern with lazy evaluation, useful for recursive pattern definitions.
    *
    * @param p
    *   a function that returns a pattern when called
    * @return
    *   a non-strict pattern that evaluates p when needed
    */
  def nonStrict(p: => Pattern): Pattern = NonStrict(() => p)

  /** Creates a pattern that tests values on the stack against a predicate.
    *
    * @param c
    *   the predicate function
    * @return
    *   a pattern that succeeds if the predicate returns true
    */
  def testValues(c: List[Any] => Boolean): Pattern = Test(c)

  /** Creates a pattern that tests the top value on the stack against a predicate.
    *
    * @param c
    *   the predicate function
    * @return
    *   a pattern that succeeds if the predicate returns true
    */
  def test[A](c: A => Boolean): Pattern = Test(values => c(values.head.asInstanceOf[A]))

  /** Creates a pattern that matches one or more repetitions of a given pattern.
    *
    * @param p
    *   the pattern to repeat
    * @return
    *   a pattern matching one or more occurrences of p
    */
  def rep1(p: Pattern): Pattern = {
    lazy val pat: Pattern = p ~ opt(nonStrict(pat))

    pat
  }

  /** Creates a right-associative pattern that matches one or more repetitions.
    *
    * This is similar to rep1 but with right-associative nesting of repetitions.
    *
    * @param p
    *   the pattern to repeat
    * @return
    *   a right-associative pattern matching one or more occurrences of p
    */
  def repr1(p: Pattern): Pattern = {
    lazy val pat: Pattern = p ~ optr(nonStrict(pat))

    pat
  }

  /** Creates a pattern that matches zero or more repetitions of a given pattern.
    *
    * @param p
    *   the pattern to repeat
    * @return
    *   a pattern matching zero or more occurrences of p
    */
  def rep(p: Pattern): Pattern = opt(rep1(p))

  /** Creates a right-associative pattern that matches zero or more repetitions.
    *
    * @param p
    *   the pattern to repeat
    * @return
    *   a right-associative pattern matching zero or more occurrences of p
    */
  def repr(p: Pattern): Pattern = optr(repr1(p))

  /** Creates a pattern that matches one or more repetitions and collects transformed values.
    *
    * @param p
    *   the pattern to repeat
    * @param arity
    *   the number of values to transform for each match
    * @param f
    *   the transformation function
    * @return
    *   a pattern that collects transformed values into a list
    */
  def rep1t(p: Pattern, arity: Int)(f: Seq[Any] => Any): Pattern =
    push(new ListBuffer[Any]) ~ rep1(p ~ transform(arity)(f) ~ action2[ListBuffer[Any], Any] { (list, item) =>
      list += item
      list
    }) ~ action[ListBuffer[Any]](_.toList)

  /** Creates a pattern that matches one or more repetitions and collects transformed values.
    *
    * @param p
    *   the pattern to repeat
    * @param f
    *   the action function to apply to each match
    * @return
    *   a pattern that collects transformed values into a list
    */
  def rep1a[A](p: Pattern)(f: A => Any): Pattern =
    push(new ListBuffer[Any]) ~ rep1(p ~ action(f) ~ action2[ListBuffer[Any], Any] { (list, item) =>
      list += item
      list
    }) ~ action[ListBuffer[Any]](_.toList)

  /** Creates a pattern that matches one or more repetitions and collects the matched values.
    *
    * @param p
    *   the pattern to repeat
    * @return
    *   a pattern that collects matched values into a list
    */
  def rep1i(p: Pattern): Pattern = rep1a[Any](p)(identity)

  /** Creates a right-associative pattern that matches one or more repetitions and collects transformed values.
    *
    * @param p
    *   the pattern to repeat
    * @param arity
    *   the number of values to transform for each match
    * @param f
    *   the transformation function
    * @return
    *   a right-associative pattern that collects transformed values into a list
    */
  def repr1t(p: Pattern, arity: Int)(f: Seq[Any] => Any): Pattern =
    push(new ListBuffer[Any]) ~ repr1(p ~ transform(arity)(f) ~ action2[ListBuffer[Any], Any] { (list, item) =>
      list += item
      list
    }) ~ action[ListBuffer[Any]](_.toList)

  /** Creates a right-associative pattern that matches one or more repetitions and collects transformed values.
    *
    * @param p
    *   the pattern to repeat
    * @param f
    *   the action function to apply to each match
    * @return
    *   a right-associative pattern that collects transformed values into a list
    */
  def repr1a[A](p: Pattern)(f: A => Any): Pattern =
    push(new ListBuffer[Any]) ~ repr1(p ~ action(f) ~ action2[ListBuffer[Any], Any] { (list, item) =>
      list += item
      list
    }) ~ action[ListBuffer[Any]](_.toList)

  /** Creates a right-associative pattern that matches one or more repetitions and collects the matched values.
    *
    * @param p
    *   the pattern to repeat
    * @return
    *   a right-associative pattern that collects matched values into a list
    */
  def repr1i(p: Pattern): Pattern = repr1a[Any](p)(identity)

  /** Creates a pattern that matches zero or more repetitions and collects transformed values.
    *
    * @param p
    *   the pattern to repeat
    * @param arity
    *   the number of values to transform for each match
    * @param f
    *   the transformation function
    * @return
    *   a pattern that collects transformed values into a list
    */
  def rept(p: Pattern, arity: Int)(f: Seq[Any] => Any): Pattern =
    push(new ListBuffer[Any]) ~ rep(p ~ transform(arity)(f) ~ action2[ListBuffer[Any], Any] { (list, item) =>
      list += item
      list
    }) ~ action[ListBuffer[Any]](_.toList)

  /** Creates a pattern that matches zero or more repetitions and collects transformed values.
    *
    * @param p
    *   the pattern to repeat
    * @param f
    *   the action function to apply to each match
    * @return
    *   a pattern that collects transformed values into a list
    */
  def repa[A](p: Pattern)(f: A => Any): Pattern =
    push(new ListBuffer[Any]) ~ rep(p ~ action(f) ~ action2[ListBuffer[Any], Any] { (list, item) =>
      list += item
      list
    }) ~ action[ListBuffer[Any]](_.toList)

  /** Creates a pattern that matches zero or more repetitions and collects the matched values.
    *
    * @param p
    *   the pattern to repeat
    * @return
    *   a pattern that collects matched values into a list
    */
  def repi(p: Pattern): Pattern = repa[Any](p)(identity)

  /** Creates a right-associative pattern that matches zero or more repetitions and collects transformed values.
    *
    * @param p
    *   the pattern to repeat
    * @param arity
    *   the number of values to transform for each match
    * @param f
    *   the transformation function
    * @return
    *   a right-associative pattern that collects transformed values into a list
    */
  def reprt(p: Pattern, arity: Int)(f: Seq[Any] => Any): Pattern =
    push(new ListBuffer[Any]) ~ repr(p ~ transform(arity)(f) ~ action2[ListBuffer[Any], Any] { (list, item) =>
      list += item
      list
    }) ~ action[ListBuffer[Any]](_.toList)

  /** Creates a right-associative pattern that matches zero or more repetitions and collects transformed values.
    *
    * @param p
    *   the pattern to repeat
    * @param f
    *   the action function to apply to each match
    * @return
    *   a right-associative pattern that collects transformed values into a list
    */
  def repra[A](p: Pattern)(f: A => Any): Pattern =
    push(new ListBuffer[Any]) ~ repr(p ~ action(f) ~ action2[ListBuffer[Any], Any] { (list, item) =>
      list += item
      list
    }) ~ action[ListBuffer[Any]](_.toList)

  /** Creates a pattern that pushes a value onto the value stack.
    *
    * @param v
    *   the value to push
    * @return
    *   a pattern that pushes the value
    */
  def push(v: Any): Pattern = Push(v)

  /** Creates a pattern that pushes the current input position onto the value stack.
    *
    * @return
    *   a pattern that captures the current position
    */
  def pointer: Pattern = Pointer

  /** Creates a pattern that captures the region matched by a pattern and applies an action.
    *
    * @param p
    *   the pattern to match
    * @param action
    *   a function that processes the start and end positions
    * @return
    *   a pattern that captures the matched region
    */
  def capture(p: Pattern)(action: (I, I) => Any): Pattern = pointer ~ p ~ pointer ~ action2[I, I](action)

  /** Creates a pattern that captures the wrapped values matched by a pattern.
    *
    * @param p
    *   the pattern to match
    * @return
    *   a pattern that captures the wrapped values
    */
  def captureWrapped(p: Pattern): Pattern = capture(p)(_.listWrapped(_))

  /** Creates a pattern that transforms values on the stack.
    *
    * @param arity
    *   the number of values to transform
    * @param f
    *   the transformation function
    * @return
    *   a pattern that applies the transformation
    */
  def transform(arity: Int)(f: Seq[Any] => Any): Pattern = Transform(arity, f)

  /** Creates a pattern that applies an action to a single value on the stack.
    *
    * @param f
    *   the action function
    * @return
    *   a pattern that applies the action
    */
  def action[A](f: A => Any): Pattern = transform(1) { case Seq(a) => f(a.asInstanceOf[A]) }

  /** Creates a pattern that applies an action to two values on the stack.
    *
    * @param f
    *   the action function
    * @return
    *   a pattern that applies the action
    */
  def action2[A, B](f: (A, B) => Any): Pattern = transform(2) { case Seq(a, b) =>
    f(a.asInstanceOf[A], b.asInstanceOf[B])
  }

  /** Creates a pattern that applies an action to three values on the stack.
    *
    * @param f
    *   the action function
    * @return
    *   a pattern that applies the action
    */
  def action3[A, B, C](f: (A, B, C) => Any): Pattern = transform(3) { case Seq(a, b, c) =>
    f(a.asInstanceOf[A], b.asInstanceOf[B], c.asInstanceOf[C])
  }

  /** Creates a pattern that applies an action to four values on the stack.
    *
    * @param f
    *   the action function
    * @return
    *   a pattern that applies the action
    */
  def action4[A, B, C, D](f: (A, B, C, D) => Any): Pattern = transform(4) { case Seq(a, b, c, d) =>
    f(a.asInstanceOf[A], b.asInstanceOf[B], c.asInstanceOf[C], d.asInstanceOf[D])
  }

  /** Positive look-behind
    *
    * @param p
    *   the predicate
    * @return
    *   a pattern that matches if there is a previous element that matches p
    */
  def lookBehind(p: E => Boolean): Pattern = Clas(input => input.prev.exists(prev => p(prev.elem)))

  /** Negative look-behind
    *
    * @param p
    *   the predicate
    * @return
    *   a pattern that matches if there is no previous element that matches p
    */
  def notLookBehind(p: E => Boolean): Pattern = Clas(input => input.prev.forall(prev => !p(prev.elem)))

  /** The core pattern type representing a pattern to match.
    */
  trait Pattern {

    /** Sequence operator: creates a pattern that matches this pattern followed by another pattern.
      *
      * @param that
      *   the pattern to match after this one
      * @return
      *   a pattern matching this pattern followed by that pattern
      */
    def ~(that: Pattern): Pattern = Sequence(this, that)

    /** Alternative operator: creates a pattern that matches either this pattern or another pattern.
      *
      * @param that
      *   the alternative pattern to try if this one fails
      * @return
      *   a pattern matching either this pattern or that pattern
      */
    def |(that: Pattern): Pattern = Alternative(this, that)
  }

  // Protected pattern implementations
  protected case object Fence                                    extends Pattern with Choice
  protected case object Cut                                      extends Pattern
  protected case object Nop                                      extends Pattern
  protected case object Fail                                     extends Pattern
  protected case object Pointer                                  extends Pattern
  protected case class Sequence(p: Pattern, q: Pattern)          extends Pattern
  protected case class Alternative(p: Pattern, q: Pattern)       extends Pattern
  protected case class Not(p: Pattern)                           extends Pattern
  protected case class Clas(c: E => Boolean)                     extends Pattern
  protected case class Match(e: Seq[E])                          extends Pattern
  protected case class Push(v: Any)                              extends Pattern
  protected case class Transform(arity: Int, f: Seq[Any] => Any) extends Pattern
  protected case class NonStrict(p: () => Pattern)               extends Pattern
  protected case class Test(p: List[Any] => Boolean)             extends Pattern

  // Backtracking-related types
  protected trait Choice
  protected case class ChoicePoint(input: I, pattern: Pattern, call: List[Pattern], value: List[Any]) extends Choice

  /** Execution limit for pattern matching, useful for debugging infinite loops. Set to Int.MaxValue by default
    * (effectively unlimited).
    */
  var runlimit: Int = Int.MaxValue

  /** Helper method for debug output when runlimit is set.
    *
    * @param s
    *   the debug message to print
    */
  private[recognizer] def debug(s: String): Unit =
    if (runlimit < Int.MaxValue)
      println(s)

  /** Represents the execution state during pattern matching.
    *
    * @param pointer
    *   the current input position
    * @param pat
    *   the initial pattern to match
    */
  class Runstate private[recognizer] (private[recognizer] var pointer: I, pat: Pattern) {
    private[recognizer] val choice              = new mutable.Stack[Choice]
    private[recognizer] var call: List[Pattern] = Nil
    private[recognizer] var value: List[Any]    = Nil
    private[recognizer] var ip: Pattern         = pat

    /** Returns the current values on the stack.
      *
      * @return
      *   the list of values on the stack
      */
    def values: List[Any] = value

    /** Pushes a pattern onto the call stack.
      *
      * @param p
      *   the pattern to push
      */
    private[recognizer] def push(p: Pattern): Unit = call = p :: call

    /** Advances to the next pattern in the call stack.
      */
    private[recognizer] def advance(): Unit =
      ip = call match {
        case h :: t =>
          call = t
          h
        case Nil => null
      }

    /** Backtracks to the most recent choice point.
      *
      * @return
      *   true if backtracking succeeded, false if no choice points remain
      */
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

  /** Runs a pattern on input and returns all possible matches.
    *
    * @param input
    *   the input to match against
    * @param pat
    *   the pattern to match
    * @return
    *   a list of all possible matches and remaining inputs
    */
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

  /** Runs a pattern on input and returns the first match.
    *
    * @param input
    *   the input to match against
    * @param pat
    *   the pattern to match
    * @return
    *   Some((value, remaining input, runstate)) if match succeeded, None if failed
    */
  def run(input: I, pat: Pattern): Option[(Option[Any], I, Runstate)] = run(new Runstate(input, pat))

  /** Continues running from an existing runstate to find the next match.
    *
    * @param state
    *   the current execution state
    * @return
    *   Some((value, remaining input, runstate)) if match succeeded, None if failed
    */
  def rerun(state: Runstate): Option[(Option[Any], I, Runstate)] =
    if (state.backtrack) run(state)
    else None

  /** Core pattern matching engine that executes patterns against input.
    *
    * @param state
    *   the current execution state
    * @return
    *   Some((value, remaining input, runstate)) if match succeeded, None if failed
    */
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
