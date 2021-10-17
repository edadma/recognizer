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

}
