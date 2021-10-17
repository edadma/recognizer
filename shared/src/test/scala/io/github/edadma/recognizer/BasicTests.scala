package io.github.edadma.recognizer

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class BasicTests extends AnyFreeSpec with Matchers with Testing {

  "basic 1" in {
    parse("a", 'a') shouldBe Some((None, ""))
  }

  "basic 2" in {
    parse("ab", 'a') shouldBe Some((None, "b"))
  }

  "basic 3" in {
    parse("b", 'a') shouldBe None
  }

  "basic 4" in {
    parse("", 'a') shouldBe None
  }

  "basic 5" in {
    parse("ab", 'a' ~ 'b') shouldBe Some((None, ""))
  }

  "basic 5a" in {
    parse("a", elem('a') | 'b') shouldBe Some((None, ""))
  }

  "basic 5b" in {
    parse("b", elem('a') | 'b') shouldBe Some((None, ""))
  }

  "basic 5c" in {
    parse("c", elem('a') | 'b') shouldBe None
  }

  "basic 5d" in {
    parse("", elem('a') | 'b') shouldBe None
  }

  "basic 5e" in {
    parse("ac", elem('a') | 'b') shouldBe Some((None, "c"))
  }

  "basic 5f" in {
    parse("bc", elem('a') | 'b') shouldBe Some((None, "c"))
  }

  "basic 6" in {
    parse("ab", (elem('a') | 'c') ~ 'b') shouldBe Some((None, ""))
  }

  "basic 7" in {
    parse("cb", (elem('a') | 'c') ~ 'b') shouldBe Some((None, ""))
  }

  "basic 8" in {
    parse("xb", (elem('a') | 'c') ~ 'b') shouldBe None
  }

  "basic 9" in {
    parse("ax", (elem('a') | 'c') ~ 'b') shouldBe None
  }

  "basic 10" in {
    parse("cx", (elem('a') | 'c') ~ 'b') shouldBe None
  }

  "basic 11" in {
    parse("a", (elem('a') | 'c') ~ 'b') shouldBe None
  }

  "basic 12" in {
    parse("c", (elem('a') | 'c') ~ 'b') shouldBe None
  }

  "basic 13" in {
    parse("", (elem('a') | 'c') ~ 'b') shouldBe None
  }

  "basic 14" in {
    parse("ab", 'a' ~ (elem('b') | 'c')) shouldBe Some((None, ""))
  }

  "basic 15" in {
    parse("abd", 'a' ~ (elem('b') | 'c')) shouldBe Some((None, "d"))
  }

  "basic 16" in {
    parse("acd", 'a' ~ (elem('b') | 'c')) shouldBe Some((None, "d"))
  }

  "basic 17" in {
    parse("ad", 'a' ~ (elem('b') | 'c')) shouldBe None
  }

  "basic 18" in {
    parse("xb", 'a' ~ (elem('b') | 'c')) shouldBe None
  }

  "basic 19" in {
    parse("xc", 'a' ~ (elem('b') | 'c')) shouldBe None
  }

  "basic 20" in {
    parse("a", 'a' ~ (elem('b') | 'c')) shouldBe None
  }

  "basic 21" in {
    parse("", 'a' ~ (elem('b') | 'c')) shouldBe None
  }

  "basic 22" in {
    parse("ab", 'a' ~ 'b' | 'c') shouldBe Some((None, ""))
  }

}
