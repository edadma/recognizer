package io.github.edadma.recognizer

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class LinkParsingTests extends AnyFreeSpec with Matchers with Testing {
  case class Link(text: String, url: String, title: Option[String])

  val ws: Pattern = rep(whitespace)
  val ws1: Pattern = rep1(whitespace)
  val link: Pattern =
    '[' ~ string(rep(noneOf(']'))) ~ ']' ~ ws ~
      '(' ~ ws ~
      ('<' ~ string(rep(noneOf('>', '\n'))) ~ '>' | not('<') ~ string(rep(noneOf(')', ' ', '\n')))) ~
      opt(ws1 ~ ('"' ~ string(rep(noneOf('"'))) ~ '"' | '\'' ~ string(rep(noneOf('\''))) ~ '\'' |
            '(' ~ string(rep(noneOf(')'))) ~ ')'),
          1)(_.head) ~ ws ~ ')' ~ action3(Link)

  "link 1" in {
    parse("[link](/uri \"title\")", link) shouldBe Some(Some(Link("link", "/uri", Some("title"))), "")
  }

  "link 2" in {
    parse("[link](/uri 'title')", link) shouldBe Some(Some(Link("link", "/uri", Some("title"))), "")
  }

  "link 3" in {
    parse("[link](/uri (title))", link) shouldBe Some(Some(Link("link", "/uri", Some("title"))), "")
  }

  "link 4" in {
    parse("[link](/uri)", link) shouldBe Some(Some(Link("link", "/uri", None)), "")
  }

  "link 5" in {
    parse("[](./target.md)", link) shouldBe Some(Some(Link("", "./target.md", None)), "")
  }

  "link 6" in {
    parse("[link]()", link) shouldBe Some(Some(Link("link", "", None)), "")
  }

  "link 7" in {
    parse("[link](<>)", link) shouldBe Some(Some(Link("link", "", None)), "")
  }

  "link 8" in {
    parse("[]()", link) shouldBe Some(Some(Link("", "", None)), "")
  }

  "link 9" in {
    parse("[link](/my uri)", link) shouldBe None
  }

  "link 10" in {
    parse("[link](</my uri>)", link) shouldBe Some(Some(Link("link", "/my uri", None)), "")
  }

  "link 11" in {
    parse("[link](<foo\nbar>)", link) shouldBe None
  }

  "link 12" in {
    parse("[a](<b)c>)", link) shouldBe Some(Some(Link("a", "b)c", None)), "")
  }

  "link 13" in {
    parse("[a](<b)c", link) shouldBe None
  }

  "link 14" in {
    parse("[a](<b)c>", link) shouldBe None
  }

  "link 15" in {
    parse("[a](<b>c)", link) shouldBe None
  }

}
