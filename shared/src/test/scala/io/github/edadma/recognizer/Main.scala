package io.github.edadma.recognizer

import pprint._

object Main extends App with Testing {

//  println(parse("adc", 'a' ~ ('b' | 'd' ~ opt('e')) ~ 'c'))
//  runlimit = 10
  println(parse("a", (elem('a') | 'c') ~ 'b'))

}
