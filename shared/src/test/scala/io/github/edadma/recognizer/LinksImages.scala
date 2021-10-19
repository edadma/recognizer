package io.github.edadma.recognizer

import scala.collection.mutable

object LinksImages extends Testing {

  case class Link(text: String, url: String, title: Option[String])
  case class LinkInfo(url: String, title: Option[String])

  val refs = new mutable.HashMap[String, LinkInfo]

  refs("bar") = LinkInfo("/url", Some("title"))

  val ws: Pattern = rep(whitespace)
  val ws1: Pattern = rep1(whitespace)
  lazy val balancedDestination: Pattern = rep(noneOf('(', ')', ' ', '\n') | '(' ~ nonStrict(balancedDestination) ~ ')')
  lazy val balancedText: Pattern = rep(noneOf('[', ']') | '[' ~ nonStrict(balancedText) ~ ']')
  val linkPattern: Pattern =
    '[' ~ string(balancedText) ~ ']' ~
      '(' ~ ws ~
      ('<' ~ string(rep(noneOf('>', '\n'))) ~ '>' | not('<') ~ string(balancedDestination)) ~
      opt(ws1 ~ ('"' ~ string(rep(noneOf('"'))) ~ '"' | '\'' ~ string(rep(noneOf('\''))) ~ '\'' | '(' ~ string(
            rep(noneOf(')'))) ~ ')'),
          1)(_.head) ~ ws ~ ')' ~ action3(Link)
  val refLinkPattern: Pattern =
    '[' ~ string(balancedText) ~ ']' ~ '[' ~ string(rep(noneOf('[', ']'))) ~
      test(values => values.nonEmpty && refs.contains(values.head.toString.toLowerCase)) ~ ']' ~ action2[String, String] {
      (t, l) =>
        val LinkInfo(url, title) = refs(l.toLowerCase)

        Link(t, url, title)
    } |
      '[' ~ string(balancedText) ~
        test(values => values.nonEmpty && refs.contains(values.head.toString.toLowerCase)) ~ ']' ~
        opt("[]") ~ action[String] { s =>
        val LinkInfo(url, title) = refs(s.toLowerCase)

        Link(s, url, title)
      }
  val link: Pattern = linkPattern | refLinkPattern
}
