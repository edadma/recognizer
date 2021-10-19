package io.github.edadma.recognizer

object LinksImages extends Testing {
  case class Link(text: String, url: String, title: Option[String])
  case class RefLink(text: Option[String], label: String)

  val ws: Pattern = rep(whitespace)
  val ws1: Pattern = rep1(whitespace)
  lazy val balancedDestination: Pattern = rep(noneOf('(', ')', ' ', '\n') | '(' ~ nonStrict(balancedDestination) ~ ')')
  lazy val balancedText: Pattern = rep(noneOf('[', ']') | '[' ~ nonStrict(balancedText) ~ ']')
  val link: Pattern =
    '[' ~ string(balancedText) ~ ']' ~
      '(' ~ ws ~
      ('<' ~ string(rep(noneOf('>', '\n'))) ~ '>' | not('<') ~ string(balancedDestination)) ~
      opt(ws1 ~ ('"' ~ string(rep(noneOf('"'))) ~ '"' | '\'' ~ string(rep(noneOf('\''))) ~ '\'' | '(' ~ string(
            rep(noneOf(')'))) ~ ')'),
          1)(_.head) ~ ws ~ ')' ~ action3(Link)
  val refLink: Pattern = '[' ~ string(balancedText) ~ ']' ~
    opt('[' ~ push("") ~ ']' | '[' ~ string(noneOf('[', ']')) ~ ']') ~ action2(RefLink)
}
