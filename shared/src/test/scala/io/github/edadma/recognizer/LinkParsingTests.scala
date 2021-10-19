package io.github.edadma.recognizer

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

object grammar extends Testing {
  case class Link(text: String, url: String, title: Option[String])

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
}

class LinkParsingTests extends AnyFreeSpec with Matchers {
  import grammar._

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

  "link 16" in {
    parse("[link](foo(and(bar)))", link) shouldBe Some(Some(Link("link", "foo(and(bar))", None)), "")
  }

  "link 17" in {
    parse("[link](foo(and(bar))", link) shouldBe None
  }

  "link 18" in {
    parse("[link](<foo(and(bar)>)", link) shouldBe Some(Some(Link("link", "foo(and(bar)", None)), "")
  }

  "link 19" in {
    parse("[link](\"title\")", link) shouldBe Some(Some(Link("link", "\"title\"", None)), "")
  }

  "link 20" in {
    parse("[link](/url\u00a0\"title\")", link) shouldBe Some(Some(Link("link", "/url\u00a0\"title\"", None)), "")
  }

  "link 21" in {
    parse("[link](/url \"title \"and\" title\")", link) shouldBe None
  }

  "link 22" in {
    parse("[link](/url 'title \"and\" title')", link) shouldBe Some(
      Some(Link("link", "/url", Some("title \"and\" title"))),
      "")
  }

  "link 23" in {
    parse("[link](   /uri\n  \"title\"  )", link) shouldBe Some(Some(Link("link", "/uri", Some("title"))), "")
  }

  "link 24" in {
    parse("[link] (/uri)", link) shouldBe None
  }

  "link 25" in {
    parse("[link [foo [bar]]](/uri)", link) shouldBe Some(Some(Link("link [foo [bar]]", "/uri", None)), "")
  }

  "link 26" in {
    parse("[link] bar](/uri)", link) shouldBe None
  }

  "link 27" in {
    parse("[link [bar](/uri)", link) shouldBe None
  }

}
