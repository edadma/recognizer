package io.github.edadma.recognizer

import scala.annotation.tailrec

object Main extends App with Testing {

  case class Link(text: String, url: String, title: Option[String])
  case class Image(text: String, url: String, title: Option[String])
  case class ImageLink(text: String, img: String, title: Option[String], url: String)

  val s = "[Example](https://example.com \"Example Title\")"
//  val s = "![Image](/path/to/image.png)"
//  val s = "[![Image](/path/to/image.png \"Example Title\")](https://example.com)"
  val space = rep(whitespace)
  val image: Pattern = "![" ~ string(rep(noneOf(']'))) ~ ']' ~ '(' ~ string(rep(noneOf(')', '"'))) ~ space ~ opt(
    '"' ~ string(rep(noneOf('"'))) ~ '"',
    1)(_.head) ~ ')' ~ action3(Image)
  val link: Pattern = '[' ~ string(rep(noneOf(']'))) ~ ']' ~ '(' ~ string(rep(noneOf(')', '"'))) ~ space ~ opt(
    '"' ~ string(rep(noneOf('"'))) ~ '"',
    1)(_.head) ~ ')' ~ action3(Link)
  val imageLink: Pattern =
    '[' ~ "![" ~ string(rep(noneOf(']'))) ~ ']' ~ '(' ~ string(rep(noneOf(')', '"'))) ~ space ~ opt(
      '"' ~ string(rep(noneOf('"'))) ~ '"',
      1)(_.head) ~ ')' ~ ']' ~ '(' ~ string(rep(noneOf(')', '"'))) ~ ')' ~ action4(ImageLink)

  runlimit = 50

//  println(parse(s, image | link | imageLink))
  println(parse("b", string(rep(not('b') ~ any))))
//  println(parse("a", string(not('b') ~ any)))

//  run(StringInput("ababab"), string(rep("ab")) ~ string(rep(any))) match {
//    case None => println("no match")
//    case Some(r) =>
//      result(r)
//
//      @tailrec
//      def result(r: (Option[Any], I, Runstate)): Unit =
//        r match {
//          case (v, u, s) =>
//            println(v, u)
//            rerun(s) match {
//              case Some(r) => result(r)
//              case None    => println("no more matches")
//            }
//        }
//  }

}
