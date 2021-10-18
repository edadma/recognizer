package io.github.edadma.recognizer

object Main extends App with Testing {

  case class Link(text: String, url: String, title: Option[String])
  case class Image(text: String, url: String, title: Option[String])
  case class ImageLink(text: String, img: String, title: Option[String], url: String)

//  val s = "[Example](https://example.com \"Example Title\")"
////  val s = "![Image](/path/to/image.png)"
////  val s = "[![Image](/path/to/image.png \"Example Title\")](https://example.com)"
//  val space = rep(whitespace)
//  val image: Pattern = "![" ~ string(rep(noneOf(']'))) ~ ']' ~ '(' ~ string(rep(noneOf(')', '"'))) ~ space ~ opt(
//    '"' ~ string(rep(noneOf('"'))) ~ '"',
//    1)(_.head) ~ ')' ~ action3(Image)
//  val link: Pattern = '[' ~ string(rep(noneOf(']'))) ~ ']' ~ '(' ~ string(rep(noneOf(')', '"'))) ~ space ~ opt(
//    '"' ~ string(rep(noneOf('"'))) ~ '"',
//    1)(_.head) ~ ')' ~ action3(Link)
//  val imageLink: Pattern =
//    '[' ~ "![" ~ string(rep(noneOf(']'))) ~ ']' ~ '(' ~ string(rep(noneOf(')', '"'))) ~ space ~ opt(
//      '"' ~ string(rep(noneOf('"'))) ~ '"',
//      1)(_.head) ~ ')' ~ ']' ~ '(' ~ string(rep(noneOf(')', '"'))) ~ ')' ~ action4(ImageLink)
//
//  println(parse(s, image | link | imageLink))

//  runlimit = 20

  println(parse("ababcd", string(rep("ab")) ~ "abcd"))

}
