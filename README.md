[![Version](https://img.shields.io/badge/version-0.0.1-orange.svg)](https://github.com/edadma/recognizer/releases)
[![Scala Version](https://img.shields.io/badge/scala-3.6.4-blue.svg)](https://www.scala-lang.org/)
[![License: ISC](https://img.shields.io/badge/License-ISC-blue.svg)](https://opensource.org/licenses/ISC)

# recognizer

A very basic backtracking pattern recognizer implemented in Scala. It provides a lightweight, composable DSL for parsing streams of input with support for backtracking, cut points, and capture transformations.

## Documentation

Complete API documentation is available at:
[https://edadma.github.io/recognizer/](https://edadma.github.io/recognizer/)

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

For cross-platform projects, ensure your `project/plugins.sbt` includes:

```scala
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.3.2")
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.5.7")
```

## Quickstart

Create a simple parser by mixing in the `CharRecognizer` and `Testing` traits:

```scala
import io.github.edadma.recognizer._

object Example extends App with CharRecognizer[Char] with Testing {
  // Define a simple calculator for addition and subtraction
  lazy val expr: Pattern = term ~ rep(('+' | '-') ~ term ~ action3[Any, Char, Any] {
    case (a, '+', b) => a.asInstanceOf[Int] + b.asInstanceOf[Int]
    case (a, '-', b) => a.asInstanceOf[Int] - b.asInstanceOf[Int]
  })
  
  lazy val term: Pattern = digits ~ action[String](_.toInt)
  
  // Test it
  val input = "123+45-67"
  parse(input, expr) match {
    case Some((Some(result), rest)) =>
      println(s"Result: $result, Remaining: '$rest'")
    case None =>
      println("Parsing failed")
  }
  // Output: Result: 101, Remaining: ''
}
```

## Common Pattern Combinators

| Combinator     | Description                                       | Example                                |
|----------------|---------------------------------------------------|----------------------------------------|
| `p ~ q`        | Sequence: match `p` then `q`                      | `digit ~ letter` matches "5a"          |
| `p | q`        | Alternation: match `p` or `q`                     | `'a' | 'b'` matches "a" or "b"         |
| `rep(p)`       | Zero-or-more repetitions of `p`                   | `rep(digit)` matches "123" or ""       |
| `rep1(p)`      | One-or-more repetitions of `p`                    | `rep1(digit)` matches "123" but not "" |
| `opt(p)`       | Optional match of `p`                             | `opt('-') ~ digit` matches "-5" or "5" |
| `not(p)`       | Negative lookahead: succeed only if `p` fails     | `letter ~ not(digit)` matches "a" but not "a5" |
| `capture(p)(f)`| Capture matched input for pattern `p`             | `capture(rep1(digit))((i, e) => i.listElem(e).mkString.toInt)` |
| `!!`           | Cut: disallow backtracking past this point        | `'a' ~ !! ~ 'b'` will fail on "ac" without trying alternatives |
| `string(p)`    | Shortcut to capture characters as string          | `string(rep1(digit))` captures digits as string |

## Advanced Examples

### Parsing a CSV Line

```scala
import io.github.edadma.recognizer._

object CSVExample extends App with CharRecognizer[Char] with Testing {
  // Define CSV patterns
  val escapedField = '"' ~ capture(rep(noneOf('"') | "\"\"" ~ action[String](_ => "\"")))((i, e) => 
    i.listElem(e).mkString) ~ '"'
  val rawField = string(rep(noneOf(',', '\n', '\r')))
  val field = escapedField | rawField
  val record = field ~ rep(',' ~ field) ~ action[List[Any]](fields => fields)
  
  // Parse a CSV line
  val input = """simple,123,"quoted,field","escaped""quote" """
  parse(input, record) match {
    case Some((Some(result), rest)) =>
      println(s"Fields: ${result.asInstanceOf[List[Any]].mkString(", ")}")
      println(s"Remaining: '$rest'")
    case None =>
      println("Parsing failed")
  }
}
```

### Creating a JSON Parser

The library is powerful enough to build parsers for common formats like JSON. See the documentation site for complete examples.

## API Overview

- **`Recognizer[W, E]`**: Core trait providing pattern combinators over input type `I = Input[W, E]`.
- **`CharRecognizer[W]`**: Specialization for character-based inputs, with helpers like `digit`, `alpha`, `ident`, `kw`, `sym`, etc.
- **`Input[W, E]`**: Represents a stream of elements `E` with wrapped values `W`; includes helpers to collect rest of input.
- **`StringInput`**: Implementation of `Input` for string parsing.
- **`Testing`**: Mixin providing a convenient `parse` method for quick tests and REPL usage.

For full details, refer to the [API documentation](https://edadma.github.io/recognizer/).

## Error Handling

The library returns `None` when parsing fails. For more detailed error reporting, you can implement your own error handling using combinators like `pointer` and `action`:

```scala
def withErrorLocation(p: Pattern): Pattern =
  pointer ~ p ~ action[Any](result => result) | 
  pointer ~ action[Input[Char, Char]](input => 
    throw new RuntimeException(s"Parse error at position ${input.listElem(StringInput("", 0)).length}")
  )
```

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