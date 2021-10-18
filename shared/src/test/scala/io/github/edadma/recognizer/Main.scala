package io.github.edadma.recognizer

object Main extends App with Testing {

  case class Link(text: String, url: String, title: Option[String])
  case class Image(text: String, url: String)
  case class ImageLink(text: String, img: String, url: String)

  val s = "[Example](https://example.com \"Example Title\")"
//  val s = "![Example](https://example.com)"
  val space = rep(whitespace)
  val image: Pattern = "![" ~ string(rep(noneOf(']'))) ~ ']' ~ '(' ~ string(rep(noneOf(')'))) ~ ')' ~ action2(Image)
  val link: Pattern = '[' ~ string(rep(noneOf(']'))) ~ ']' ~ '(' ~ string(rep(noneOf(')', '"'))) ~ space ~ opt(
    '"' ~ string(rep(noneOf('"'))) ~ '"',
    1)(_.head) ~ ')' ~ action3(Link)

  println(parse(s, link))

}
