[![Version](https://img.shields.io/badge/version-0.0.1-orange.svg)](https://github.com/edadma/recognizer/releases)
[![Scala Version](https://img.shields.io/badge/scala-3.6.4-blue.svg)](https://www.scala-lang.org/)
[![License: ISC](https://img.shields.io/badge/License-ISC-blue.svg)](https://opensource.org/licenses/ISC)

# recognizer

A very basic backtracking pattern recognizer implemented in Scala. It provides a lightweight, composable DSL for parsing streams of input with support for backtracking, cut points, and capture transformations.

## Features

- **Cross-platform**: Supports JVM, Scala.js, and Scala Native via a cross-project setup
- **Composable DSL**: Build complex patterns using sequence (`~`), alternation (`|`), repetition (`rep`, `rep1`), optional (`opt`), negation (`not`), and more
- **Backtracking & Cut**: Fine-grained control over backtracking with the cut operator (`!!`)
- **Value Capture & Transform**: Capture matched input positions or values and apply custom transformations
- **Built-in Patterns**: Common patterns for letters, digits, identifiers, whitespace, links, images, etc.
- **Lightweight**: No external dependencies beyond Scala standard library and ScalaTest for testing

## Installation

Add the following to your `build.sbt`:

```scala
libraryDependencies += "io.github.edadma" %%% "recognizer" % "0.0.1"
```

Ensure your `project/plugins.sbt` includes cross-project support:

```scala
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.3.2")
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.5.7")
```

## Quickstart

Create a simple parser by mixing in the `Testing` trait:

```scala
import io.github.edadma.recognizer._
import io.github.edadma.recognizer.Testing

object Example extends App with Testing {
  // Pattern: one or more digits followed by a letter
  val pattern: Pattern = rep1(digit) ~ letter

  parse("123a", pattern) match {
    case Some((v, rest)) =>
      println(s"Parsed value: $v, remaining: '$rest'")
    case None =>
      println("Parsing failed")
  }
}
```

### Common Pattern Primitives

| Primitive     | Description                                       |
|---------------|---------------------------------------------------|
| `p ~ q`       | Sequence: match `p` then `q`                      |
| `p | q`       | Alternation: match `p` or `q`                     |
| `rep(p)`      | Zero-or-more repetitions of `p`                   |
| `rep1(p)`     | One-or-more repetitions of `p`                    |
| `opt(p)`      | Optional match of `p`                             |
| `not(p)`      | Negative lookahead: succeed only if `p` fails     |
| `pointer`     | Push current input position onto the value stack  |
| `!!`          | Cut: disallow backtracking past this point        |
| `action(f)`   | Apply transform `f` to captured values            |

## API Overview

- **`Recognizer[W, E]`**: Core trait providing pattern combinators over input type `I = Input[W, E]`.
- **`CharRecognizer[W]`**: Specialization for character-based inputs, with helpers like `digit`, `alpha`, `ident`, `kw`, `sym`, etc.
- **`Input[W, E]`**: Represents a stream of elements `E` with wrapped values `W`; includes helpers to collect rest of input.
- **`Testing`**: Mixin providing a convenient `parse` method for quick tests and REPL usage.

For full details, refer to the Scaladocs or browse the source under `shared/src/main/scala/io/github/edadma/recognizer`.

## Testing

Unit tests are written with ScalaTest. Run them with:

```bash
sbt test
```

## Contributing

Contributions are welcome! To contribute:

1. Fork the repository
2. Create a feature branch: `git checkout -b feature/YourFeature`
3. Commit your changes and push to your fork
4. Open a Pull Request against the `main` branch

Please follow the existing code style and include tests for new features.

## License

This project is licensed under the **ISC License**. See the [LICENSE](LICENSE) file for details.

