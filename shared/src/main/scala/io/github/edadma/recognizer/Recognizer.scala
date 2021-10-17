package io.github.edadma.recognizer

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.{implicitConversions, postfixOps}

abstract class Recognizer[E, V] {

  implicit def elem(e: E): Elem = Elem(e)

  trait Pattern {
    def ~(that: Pattern): And = And(this, that)
    def |(that: Pattern): Or = Or(this, that)
  }

  case class And(p: Pattern, q: Pattern) extends Pattern
  case class Or(p: Pattern, q: Pattern) extends Pattern
  case class Elem(e: E) extends Pattern
  case class Push(v: V) extends Pattern
  case class Transform(argc: Int, f: Seq[V] => V) extends Pattern

  def parse(input: Input[E], pat: Pattern): Option[(Option[V], Input[E])] = {
    case class Choice(input: Input[E], pattern: Pattern, call: List[Pattern])
    var call: List[Pattern] = Nil
    val choice = new mutable.Stack[Choice]
    val value = new mutable.Stack[V]
    var pointer = input

    def push(p: Pattern): Unit = call = p :: call

    def pop: Pattern =
      call match {
        case h :: t =>
          call = t
          h
      }

    push(pat)

    var limit = 10

    @tailrec
    def parse(): Boolean = {
      limit -= 1

      if (limit < 0) return false

      if (call.nonEmpty) {
        println("next", call.head, pointer)
        pop match {
          case Or(p, q) =>
            choice push Choice(pointer, q, call)
            push(p)
            parse()
          case Elem(e) =>
            if (!pointer.eoi && pointer.elem == e) {
              pointer = pointer.next
              parse()
            } else
              choice headOption match {
                case None => false
                case Some(Choice(p, n, c)) =>
                  pointer = p
                  call = n :: c
                  parse()
              }
          case Push(v) =>
            value push v
            parse()
          //          case Transform(argc, f) =>
          case And(p, q) =>
            push(q)
            push(p)
            parse()
        }
      } else
        true
    }

    if (parse()) Some((value.headOption, pointer))
    else None
  }

}
