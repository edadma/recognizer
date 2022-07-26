package io.github.edadma.recognizer

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ImageParsingTests extends AnyFreeSpec with Matchers {

  import LinksImages._

  "image 1" in {
    parse("![image](/uri \"title\")", testPattern) shouldBe Some(Some(Image("image", "/uri", Some("title"))), "")
  }

  "image 2" in {
    parse("![image](/uri 'title')", testPattern) shouldBe Some(Some(Image("image", "/uri", Some("title"))), "")
  }

  "image 3" in {
    parse("![image](/uri (title))", testPattern) shouldBe Some(Some(Image("image", "/uri", Some("title"))), "")
  }

  "image 4" in {
    parse("![image](/uri)", testPattern) shouldBe Some(Some(Image("image", "/uri", None)), "")
  }

  "image 5" in {
    parse("![](./target.md)", testPattern) shouldBe Some(Some(Image("", "./target.md", None)), "")
  }

  "image 6" in {
    parse("![image]()", testPattern) shouldBe Some(Some(Image("image", "", None)), "")
  }

  "image 7" in {
    parse("![image](<>)", testPattern) shouldBe Some(Some(Image("image", "", None)), "")
  }

  "image 8" in {
    parse("![]()", testPattern) shouldBe Some(Some(Image("", "", None)), "")
  }

  "image 9" in {
    parse("![image](/my uri)", testPattern) shouldBe None
  }

  "image 10" in {
    parse("![image](</my uri>)", testPattern) shouldBe Some(Some(Image("image", "/my uri", None)), "")
  }

  "image 11" in {
    parse("![image](<foo\nbar>)", testPattern) shouldBe None
  }

  "image 12" in {
    parse("![a](<b)c>)", testPattern) shouldBe Some(Some(Image("a", "b)c", None)), "")
  }

  "image 13" in {
    parse("![a](<b)c", testPattern) shouldBe None
  }

  "image 14" in {
    parse("![a](<b)c>", testPattern) shouldBe None
  }

  "image 15" in {
    parse("![a](<b>c)", testPattern) shouldBe None
  }

  "image 16" in {
    parse("![image](foo(and(bar)))", testPattern) shouldBe Some(Some(Image("image", "foo(and(bar))", None)), "")
  }

  "image 17" in {
    parse("![image](foo(and(bar))", testPattern) shouldBe None
  }

  "image 18" in {
    parse("![image](<foo(and(bar)>)", testPattern) shouldBe Some(Some(Image("image", "foo(and(bar)", None)), "")
  }

  "image 19" in {
    parse("![image](\"title\")", testPattern) shouldBe Some(Some(Image("image", "\"title\"", None)), "")
  }

  "image 20" in {
    parse("![image](/url\u00a0\"title\")", testPattern) shouldBe Some(Some(Image("image", "/url\u00a0\"title\"", None)),
                                                                      "")
  }

  "image 21" in {
    parse("![image](/url \"title \"and\" title\")", testPattern) shouldBe None
  }

  "image 22" in {
    parse("![image](/url 'title \"and\" title')", testPattern) shouldBe Some(
      Some(Image("image", "/url", Some("title \"and\" title"))),
      "")
  }

  "image 23" in {
    parse("![image](   /uri\n  \"title\"  )", testPattern) shouldBe Some(Some(Image("image", "/uri", Some("title"))),
                                                                         "")
  }

  "image 24" in {
    parse("![image] (/uri)", testPattern) shouldBe None
  }

  "image 25" in {
    parse("![image [foo [bar]]](/uri)", testPattern) shouldBe Some(Some(Image("image [foo [bar]]", "/uri", None)), "")
  }

  "image 26" in {
    parse("![image] bar](/uri)", testPattern) shouldBe None
  }

  "image 27" in {
    parse("![image [bar](/uri)", testPattern) shouldBe None
  }

  "image ref 1" in {
    parse("![foo][bar]", testPattern) shouldBe Some(Some(Image("foo", "/url", Some("title"))), "")
  }

  "image ref 2" in {
    parse("![image [foo [bar]]][ref]", testPattern) shouldBe Some(Some(Image("image [foo [bar]]", "/uri", None)), "")
  }

  "image ref 3" in {
    parse("![foo][BaR]", testPattern) shouldBe Some(Some(Image("foo", "/url", Some("title"))), "")
  }

  "image ref 4" in {
    parse("![foo] [bar]", testPattern) shouldBe None
  }

  "image ref 5" in {
    parse("![foo]\n[bar]", testPattern) shouldBe None
  }

  "image ref 6" in {
    parse("![foo][ref[]", testPattern) shouldBe None
  }

  "image ref 7" in {
    parse("![]", testPattern) shouldBe None
  }

  "image ref 8" in {
    parse("![\n]", testPattern) shouldBe None
  }

  "image ref 9" in {
    parse("![fooo][]", testPattern) shouldBe Some(Some(Image("fooo", "/url", Some("title"))), "")
  }

  "image ref 10" in {
    parse("![Fooo][]", testPattern) shouldBe Some(Some(Image("Fooo", "/url", Some("title"))), "")
  }

  "image ref 11" in {
    parse("![fooo]\n[]", testPattern) shouldBe Some(Some(Image("fooo", "/url", Some("title"))), "\n[]")
  }

  "image ref 12" in {
    parse("![fooo]", testPattern) shouldBe Some(Some(Image("fooo", "/url", Some("title"))), "")
  }

  "image ref 13" in {
    parse("![[bar [foo]", testPattern) shouldBe None
  }

  "image ref 14" in {
    parse("![Fooo]", testPattern) shouldBe Some(Some(Image("Fooo", "/url", Some("title"))), "")
  }

  "image ref 15" in {
    parse("![fooo] bar", testPattern) shouldBe Some(Some(Image("fooo", "/url", Some("title"))), " bar")
  }

}
