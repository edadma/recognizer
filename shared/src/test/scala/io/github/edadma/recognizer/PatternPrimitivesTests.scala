package io.github.edadma.recognizer

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class PatternPrimitivesTests extends AnyFreeSpec with Matchers with Testing {

  "opt 1" in {
    parse("a", opt('a')) shouldBe Some((None, ""))
  }

  "opt 2" in {
    parse("b", opt('a')) shouldBe Some((None, "b"))
  }

  "opt 3" in {
    parse("", opt('a')) shouldBe Some((None, ""))
  }

  "rep 1" in {
    parse("a", rep('a')) shouldBe Some((None, ""))
  }

  "rep 2" in {
    parse("aaa", rep('a')) shouldBe Some((None, ""))
  }

  "rep 3" in {
    parse("", rep('a')) shouldBe Some((None, ""))
  }

  "rep 4" in {
    parse("asdf", rep('a')) shouldBe Some((None, "sdf"))
  }

  "rep 5" in {
    parse("sdf", rep('a')) shouldBe Some((None, "sdf"))
  }

  "rep 6" in {
    parse("aaaasdf", rep('a')) shouldBe Some((None, "sdf"))
  }

  "rep 7" in {
    parse("abababasdf", rep('a' ~ 'b')) shouldBe Some((None, "asdf"))
  }

  "rep 8" in {
    parse("asdf", rep('a' ~ 'b')) shouldBe Some((None, "asdf"))
  }

}
