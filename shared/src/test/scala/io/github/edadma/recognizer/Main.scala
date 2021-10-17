package io.github.edadma.recognizer

import pprint._

object Main extends App with Testing {

//  println(parse("adc", 'a' ~ ('b' | 'd' ~ opt('e')) ~ 'c'))
  runlimit = 15
  println(parse("a", rep1('a' ~ push(3), 1)(_.head)))
//  println(parse("b", opt('a' ~ push(3), 1)(_.head)))

}
