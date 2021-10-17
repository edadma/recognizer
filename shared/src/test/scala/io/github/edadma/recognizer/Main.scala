package io.github.edadma.recognizer

import pprint._

object Main extends App with Testing {

//  println(parse("adc", 'a' ~ ('b' | 'd' ~ opt('e')) ~ 'c'))
//  runlimit = 30
  println(parse("ab", capture(rep(elem('a') | 'b'))))
//  println(parse("b", opt('a' ~ push(3), 1)(_.head)))

}
