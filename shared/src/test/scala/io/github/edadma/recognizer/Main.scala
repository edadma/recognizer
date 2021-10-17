package io.github.edadma.recognizer

import scala.annotation.tailrec
import scala.language.{implicitConversions, postfixOps}
import scala.collection.mutable
import pprint._

object Main extends App with Testing {

  object M extends Recognizer[Char] {

//    val pattern = Seq(Elem('a'), Seq(Alt(Elem('b'), Elem('d')), Elem('c')))
//    val pattern = Elem('a')
//    val pattern = Alt(Elem('a'), Elem('b'))
    val pattern = 'a' ~ ('b' | 'd' ~ opt('e')) ~ 'c'

    def parse(s: String): Option[(Option[Any], StringInput)] = run(new StringInput(s), pattern)
  }

  println(M.parse("adc"))

}
