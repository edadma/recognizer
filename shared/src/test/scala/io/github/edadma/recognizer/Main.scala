package io.github.edadma.recognizer

import scala.annotation.tailrec
import scala.language.{implicitConversions, postfixOps}
import scala.collection.mutable
import pprint._

object Main extends App {

  object M extends Recognizer[Char, Any] {
    class StringInput(s: String, idx: Int) extends Input[Char] {
      def eoi: Boolean = idx >= s.length

      def elem: Char = s(idx)

      def next: StringInput = new StringInput(s, idx + 1)

      override def toString: String = s"StringInput<${s.substring(idx)}>"
    }

    def input(s: String): StringInput = new StringInput(s, 0)

//    val pattern = Seq(Elem('a'), Seq(Alt(Elem('b'), Elem('d')), Elem('c')))
//    val pattern = Elem('a')
//    val pattern = Alt(Elem('a'), Elem('b'))
    val pattern = 'a' ~ ('b' | 'd' ~ opt('e')) ~ 'c'

    pprintln(pattern)
  }

  println(M.parse(M.input("adc"), M.pattern))

}
