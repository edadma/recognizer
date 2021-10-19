package io.github.edadma.recognizer

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class LinkParsingTests extends AnyFreeSpec with Matchers {

  import LinksImages._

  "link 1" in {
    parse("[link](/uri \"title\")", testPattern) shouldBe Some(Some(Link("link", "/uri", Some("title"))), "")
  }

  "link 2" in {
    parse("[link](/uri 'title')", testPattern) shouldBe Some(Some(Link("link", "/uri", Some("title"))), "")
  }

  "link 3" in {
    parse("[link](/uri (title))", testPattern) shouldBe Some(Some(Link("link", "/uri", Some("title"))), "")
  }

  "link 4" in {
    parse("[link](/uri)", testPattern) shouldBe Some(Some(Link("link", "/uri", None)), "")
  }

  "link 5" in {
    parse("[](./target.md)", testPattern) shouldBe Some(Some(Link("", "./target.md", None)), "")
  }

  "link 6" in {
    parse("[link]()", testPattern) shouldBe Some(Some(Link("link", "", None)), "")
  }

  "link 7" in {
    parse("[link](<>)", testPattern) shouldBe Some(Some(Link("link", "", None)), "")
  }

  "link 8" in {
    parse("[]()", testPattern) shouldBe Some(Some(Link("", "", None)), "")
  }

  "link 9" in {
    parse("[link](/my uri)", testPattern) shouldBe None
  }

  "link 10" in {
    parse("[link](</my uri>)", testPattern) shouldBe Some(Some(Link("link", "/my uri", None)), "")
  }

  "link 11" in {
    parse("[link](<foo\nbar>)", testPattern) shouldBe None
  }

  "link 12" in {
    parse("[a](<b)c>)", testPattern) shouldBe Some(Some(Link("a", "b)c", None)), "")
  }

  "link 13" in {
    parse("[a](<b)c", testPattern) shouldBe None
  }

  "link 14" in {
    parse("[a](<b)c>", testPattern) shouldBe None
  }

  "link 15" in {
    parse("[a](<b>c)", testPattern) shouldBe None
  }

  "link 16" in {
    parse("[link](foo(and(bar)))", testPattern) shouldBe Some(Some(Link("link", "foo(and(bar))", None)), "")
  }

  "link 17" in {
    parse("[link](foo(and(bar))", testPattern) shouldBe None
  }

  "link 18" in {
    parse("[link](<foo(and(bar)>)", testPattern) shouldBe Some(Some(Link("link", "foo(and(bar)", None)), "")
  }

  "link 19" in {
    parse("[link](\"title\")", testPattern) shouldBe Some(Some(Link("link", "\"title\"", None)), "")
  }

  "link 20" in {
    parse("[link](/url\u00a0\"title\")", testPattern) shouldBe Some(Some(Link("link", "/url\u00a0\"title\"", None)), "")
  }

  "link 21" in {
    parse("[link](/url \"title \"and\" title\")", testPattern) shouldBe None
  }

  "link 22" in {
    parse("[link](/url 'title \"and\" title')", testPattern) shouldBe Some(
      Some(Link("link", "/url", Some("title \"and\" title"))),
      "")
  }

  "link 23" in {
    parse("[link](   /uri\n  \"title\"  )", testPattern) shouldBe Some(Some(Link("link", "/uri", Some("title"))), "")
  }

  "link 24" in {
    parse("[link] (/uri)", testPattern) shouldBe None
  }

  "link 25" in {
    parse("[link [foo [bar]]](/uri)", testPattern) shouldBe Some(Some(Link("link [foo [bar]]", "/uri", None)), "")
  }

  "link 26" in {
    parse("[link] bar](/uri)", testPattern) shouldBe None
  }

  "link 27" in {
    parse("[link [bar](/uri)", testPattern) shouldBe None
  }

  "ref 1" in {
    parse("[foo][bar]", testPattern) shouldBe Some(Some(Link("foo", "/url", Some("title"))), "")
  }

  "ref 2" in {
    parse("[link [foo [bar]]][ref]", testPattern) shouldBe Some(Some(Link("link [foo [bar]]", "/uri", None)), "")
  }

  "ref 3" in {
    parse("[foo][BaR]", testPattern) shouldBe Some(Some(Link("foo", "/url", Some("title"))), "")
  }

  "ref 4" in {
    parse("[foo] [bar]", testPattern) shouldBe None
  }

  "ref 5" in {
    parse("[foo]\n[bar]", testPattern) shouldBe None
  }

  "ref 6" in {
    parse("[foo][ref[]", testPattern) shouldBe None
  }

  "ref 7" in {
    parse("[]", testPattern) shouldBe None
  }

  "ref 8" in {
    parse("[\n]", testPattern) shouldBe None
  }

  "ref 9" in {
    parse("[fooo][]", testPattern) shouldBe Some(Some(Link("fooo", "/url", Some("title"))), "")
  }

  "ref 10" in {
    parse("[Fooo][]", testPattern) shouldBe Some(Some(Link("Fooo", "/url", Some("title"))), "")
  }

  "ref 11" in {
    parse("[fooo]\n[]", testPattern) shouldBe Some(Some(Link("fooo", "/url", Some("title"))), "\n[]")
  }

  "ref 12" in {
    parse("[fooo]", testPattern) shouldBe Some(Some(Link("fooo", "/url", Some("title"))), "")
  }

  "ref 13" in {
    parse("[[bar [foo]", testPattern) shouldBe None
  }

  "ref 14" in {
    parse("[Fooo]", testPattern) shouldBe Some(Some(Link("Fooo", "/url", Some("title"))), "")
  }

  "ref 15" in {
    parse("[fooo] bar", testPattern) shouldBe Some(Some(Link("fooo", "/url", Some("title"))), " bar")
  }

}
