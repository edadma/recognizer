package io.github.edadma.recognizer

import pprint._

object Main extends App with Testing {

//  println(parse("adc", 'a' ~ ('b' | 'd' ~ opt('e')) ~ 'c'))
//  runlimit = 30
//  println(parse("ab", rep(elem('a') | 'b') ~ string(rep(any)) ~))
//  println(parse("abcd", 'a' ~ capture(Seq('b', 'c') | 'e') ~ 'd'))
  println(parse("abcd", 'a' ~ string("bc" | 'e') ~ 'd'))

}
