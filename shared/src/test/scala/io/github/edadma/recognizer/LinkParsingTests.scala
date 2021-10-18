package io.github.edadma.recognizer

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class LinkParsingTests extends AnyFreeSpec with Matchers with Testing {
  case class Link(text: String, url: String, title: Option[String])

  val ws: Pattern = rep(whitespace)
  val ws1: Pattern = rep1(whitespace)
  val link: Pattern =
    '[' ~ string(rep(noneOf(']'))) ~ ']' ~
      '(' ~ string(rep(not(anyOf(')', '"') | whitespace))) ~ ws ~
      opt('"' ~ string(rep(noneOf('"'))) ~ '"', 1)(_.head) ~ ')' ~
      action3(Link)

  "link 1" in {
    parse("[link](/uri \"title\")", link) shouldBe Some(Some(Link("link", "/uri", Some("title"))), "")
  }

}
