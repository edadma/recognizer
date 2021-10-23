package io.github.edadma.recognizer

import scala.collection.mutable

object LinksImages extends Testing {

  case class Link(text: String, url: String, title: Option[String])
  case class Image(text: String, url: String, title: Option[String])
  case class LinkInfo(url: String, title: Option[String])

  val refs = new mutable.HashMap[String, LinkInfo]

  refs("bar") = LinkInfo("/url", Some("title"))
  refs("ref") = LinkInfo("/uri", None)
  refs("ref[") = LinkInfo("/uri", None)
  refs("") = LinkInfo("/uri", None)
  refs("\n") = LinkInfo("/uri", None)
  refs("fooo") = LinkInfo("/url", Some("title"))

  lazy val balancedDestination: Pattern = rep(noneOf('(', ')', ' ', '\n') | '(' ~ nonStrict(balancedDestination) ~ ')')
  lazy val balancedText: Pattern = rep(noneOf('[', ']') | '[' ~ nonStrict(balancedText) ~ ']')
  lazy val balancedText1: Pattern = rep1(noneOf('[', ']') | '[' ~ nonStrict(balancedText) ~ ']')
  val linkPattern: Pattern =
    '[' ~ string(balancedText) ~ ']' ~
      '(' ~ ws ~
      ('<' ~ string(rep(noneOf('>', '\n'))) ~ '>' | not('<') ~ string(balancedDestination)) ~
      optt(ws1 ~ ('"' ~ string(rep(noneOf('"'))) ~ '"' | '\'' ~ string(rep(noneOf('\''))) ~ '\'' | '(' ~ string(
             rep(noneOf(')'))) ~ ')'),
           1)(_.head) ~ ws ~ ')' ~ action3(Link)
  val refLinkPattern: Pattern =
    '[' ~ string(balancedText) ~ ']' ~ '[' ~ string(rep1(noneOf('[', ']'))) ~
      test(
        values =>
          values.nonEmpty && values.head.toString.exists(!_.isWhitespace) && refs.contains(
            values.head.toString.toLowerCase)) ~ ']' ~ action2[String, String] { (t, l) =>
      val LinkInfo(url, title) = refs(l.toLowerCase)

      Link(t, url, title)
    } |
      '[' ~ string(balancedText1) ~
        test(
          values =>
            values.nonEmpty && values.head.toString.exists(!_.isWhitespace) && refs.contains(
              values.head.toString.toLowerCase)) ~ ']' ~
        opt("[]") ~ action[String] { s =>
        val LinkInfo(url, title) = refs(s.toLowerCase)

        Link(s, url, title)
      }
  val imagePattern: Pattern =
    "![" ~ string(balancedText) ~ ']' ~
      '(' ~ ws ~
      ('<' ~ string(rep(noneOf('>', '\n'))) ~ '>' | not('<') ~ string(balancedDestination)) ~
      optt(ws1 ~ ('"' ~ string(rep(noneOf('"'))) ~ '"' | '\'' ~ string(rep(noneOf('\''))) ~ '\'' | '(' ~ string(
             rep(noneOf(')'))) ~ ')'),
           1)(_.head) ~ ws ~ ')' ~ action3(Image)
  val refImagePattern: Pattern =
    "![" ~ string(balancedText) ~ ']' ~ '[' ~ string(rep1(noneOf('[', ']'))) ~
      test(
        values =>
          values.nonEmpty && values.head.toString.exists(!_.isWhitespace) && refs.contains(
            values.head.toString.toLowerCase)) ~ ']' ~ action2[String, String] { (t, l) =>
      val LinkInfo(url, title) = refs(l.toLowerCase)

      Image(t, url, title)
    } |
      "![" ~ string(balancedText1) ~
        test(
          values =>
            values.nonEmpty && values.head.toString.exists(!_.isWhitespace) && refs.contains(
              values.head.toString.toLowerCase)) ~ ']' ~
        opt("[]") ~ action[String] { s =>
        val LinkInfo(url, title) = refs(s.toLowerCase)

        Image(s, url, title)
      }
  val testPattern: Pattern = linkPattern | refLinkPattern | imagePattern | refImagePattern
}
