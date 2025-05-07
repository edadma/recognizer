package io.github.edadma.recognizer

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class LookBehindTests extends AnyFreeSpec with Matchers with Testing {

  "lookBehind" - {
    "matches when previous character satisfies predicate" in {
      parse("ab", 'a' ~ lookBehind(_ == 'a') ~ 'b') shouldBe Some((None, ""))
    }

    "fails when previous character doesn't satisfy predicate" in {
      parse("ab", 'a' ~ lookBehind(_ == 'x') ~ 'b') shouldBe None
    }

    "fails at beginning of input" in {
      parse("ab", lookBehind(_ == 'x') ~ 'a' ~ 'b') shouldBe None
    }

    "can check for specific character classes" in {
      parse("1a", digit ~ lookBehind(_.isDigit) ~ alpha) shouldBe Some((None, ""))
    }

    "can be used in longer sequences" in {
      parse("abcde", 'a' ~ 'b' ~ 'c' ~ lookBehind(_ == 'c') ~ 'd' ~ 'e') shouldBe Some((None, ""))
    }
  }

  "notLookBehind" - {
    "matches when previous character doesn't satisfy predicate" in {
      parse("ab", 'a' ~ notLookBehind(_ == 'x') ~ 'b') shouldBe Some((None, ""))
    }

    "fails when previous character satisfies predicate" in {
      parse("ab", 'a' ~ notLookBehind(_ == 'a') ~ 'b') shouldBe None
    }

    "succeeds at beginning of input" in {
      parse("ab", notLookBehind(_ == 'x') ~ 'a' ~ 'b') shouldBe Some((None, ""))
    }

    "can check for absence of character classes" in {
      parse("ab", 'a' ~ notLookBehind(_.isDigit) ~ 'b') shouldBe Some((None, ""))
    }

    "can be used in longer sequences" in {
      parse("abcde", 'a' ~ 'b' ~ 'c' ~ notLookBehind(_ == 'x') ~ 'd' ~ 'e') shouldBe Some((None, ""))
    }
  }

  "complex scenarios" - {
    "combining look-behind with other features" in {
      parse("aabc", rep1('a') ~ lookBehind(_ == 'a') ~ 'b' ~ 'c') shouldBe Some((None, ""))
    }

    "using look-behind with alternatives" in {
      parse("abc", 'a' ~ (lookBehind(_ == 'a') | lookBehind(_ == 'x')) ~ 'b' ~ 'c') shouldBe Some((None, ""))
    }

    "chaining multiple look-behind checks" in {
      parse("abc", 'a' ~ lookBehind(_ == 'a') ~ notLookBehind(_ == 'x') ~ 'b' ~ 'c') shouldBe Some((None, ""))
    }
  }

  "practical examples" - {
    "detecting word beginnings" in {
      // Match a word that begins after a space
      val wordAfterSpace = ws1 ~ lookBehind(_.isWhitespace) ~ rep1(alpha)
      parse(" word", wordAfterSpace) shouldBe Some((None, ""))
    }
  }
}
