package io.github.edadma.recognizer

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class BasicTests extends AnyFreeSpec with Matchers with Testing {

  "basic 1" in {
    parse("a", 'a') shouldBe Some((None, ""))
  }

}
