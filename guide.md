# Recognizer Library - Programmer's Guide

## Table of Contents
1. [Introduction](#introduction)
2. [Core Abstractions](#core-abstractions)
3. [Pattern Primitives](#pattern-primitives)
4. [Pattern Combinators](#pattern-combinators)
5. [Value Capture and Transformation](#value-capture-and-transformation)
6. [Backtracking Control](#backtracking-control)
7. [String and Character Processing](#string-and-character-processing)
8. [Common Patterns and Idioms](#common-patterns-and-idioms)
9. [Working Examples](#working-examples)
10. [API Reference](#api-reference)

## Introduction

The `recognizer` library is a lightweight, backtracking pattern recognizer implemented in Scala. It provides a composable DSL for parsing streams of input (such as character sequences) with support for backtracking, cut points, and value transformation.

The library is designed around the concept of pattern matching through a recursive descent parser with explicit backtracking control. It's particularly well-suited for building parsers for domain-specific languages, configuration formats, or any structured text format.

### Key Features

- Cross-platform: JVM, Scala.js, and Scala Native
- Composable pattern matching DSL
- Explicit backtracking control
- Value capture and transformation
- No external dependencies

### Basic Usage

```scala
import io.github.edadma.recognizer._

object Example extends Testing {
  val pattern: Pattern = rep1(digit) ~ alpha // match one or more digits followed by a letter
  
  val result = parse("123a", pattern)
  // result: Some((None, ""))
}
```

## Core Abstractions

### Recognizer[W, E]

The central trait that provides pattern matching functionality over an input stream of elements of type `E` with wrapped values of type `W`.

```scala
trait Recognizer[W, E] {
  type I = Input[W, E]
  
  // Pattern matching primitives and combinators
  def elem(e: E): Pattern
  def clas(c: E => Boolean): Pattern
  def not(p: Pattern): Pattern
  def rep(p: Pattern): Pattern
  def rep1(p: Pattern): Pattern
  def opt(p: Pattern): Pattern
  // ...
}
```

### Input[W, E]

Represents a stream of input elements with possible wrapped values. Provides methods to access the current element, advance to the next, and check for end of input.

```scala
trait Input[W, E] {
  def eoi: Boolean         // End of input indicator
  def elem: E              // Current element
  def wrapped: W           // Wrapped value associated with current element
  def next: Input[W, E]    // Next input position
  def rest: List[E]        // Remaining elements from current position
  def listElem(end: Input[W, E]): List[E]     // Elements between current position and end
  def listWrapped(end: Input[W, E]): List[W]  // Wrapped values between current position and end
}
```

### CharRecognizer[W]

A specialization of `Recognizer` for character input, providing additional character-based patterns:

```scala
trait CharRecognizer[W] extends Recognizer[W, Char] {
  implicit def str(s: String): Pattern   // Match a literal string
  def kw(s: String): Pattern             // Match a keyword followed by non-alphanumeric and whitespace
  def sym(s: String): Pattern            // Match a symbol followed by whitespace
  
  // Built-in patterns
  val alpha: Pattern      // Matches a letter
  val alphanum: Pattern   // Matches a letter or digit
  val digit: Pattern      // Matches a digit
  val digits: Pattern     // Matches one or more digits
  val whitespace: Pattern // Matches a whitespace character
  val ws: Pattern         // Matches zero or more whitespace
  val ws1: Pattern        // Matches one or more whitespace
  val ident: Pattern      // Matches an identifier (letter or underscore followed by alphanumerics or underscores)
  val number: Pattern     // Matches numeric literals
}
```

### Pattern

The core type representing a pattern to match:

```scala
trait Pattern {
  def ~(that: Pattern): Pattern  // Sequence: this pattern followed by that pattern
  def |(that: Pattern): Pattern  // Alternative: this pattern or that pattern
}
```

## Pattern Primitives

### Basic Element Matching

```scala
// Match a single element
elem('a')      // Match the character 'a'

// Match any element satisfying a predicate
clas(_.isDigit)  // Match any digit
clas(c => c == 'a' || c == 'b')  // Match 'a' or 'b'

// Convenience methods for character sets
anyOf('a', 'b', 'c')  // Match any of these characters
noneOf('\n', '\r')    // Match any character except these
any                   // Match any character
```

### Special Patterns

```scala
nop    // Always succeeds without consuming input
failed // Always fails
!!     // Cut operator - disallows backtracking past this point
fence  // Marker for backtracking control
```

## Pattern Combinators

### Sequence and Alternative

```scala
// Sequence: match p then q
val p: Pattern = ...
val q: Pattern = ...
val seq = p ~ q

// Alternative: match p or q
val alt = p | q
```

### Repetition

```scala
// Zero or more repetitions
rep(digit)  // Match any number of digits, including none

// One or more repetitions
rep1(digit)  // Match at least one digit

// Right-associative repetition variants
repr(p)   // Like rep, but right-associative
repr1(p)  // Like rep1, but right-associative
```

### Optional Patterns

```scala
// Optional match
opt(digit)  // Match a digit if present, succeed even if not

// Optional with transformation
optt(p, arity)(transform)  // Apply transform if pattern matches, return None if it doesn't
opti(p)  // Optional pattern that preserves the matched value

// Right-associative optional variants
optr(p)     // Like opt, but right-associative
optri(p)    // Like opti, but right-associative
```

### Negation and Lookahead

```scala
// Negative lookahead - succeed only if pattern fails
not(digit)  // Succeed if next character is not a digit

// Typical usage: ensure a pattern is not followed by something
val word = rep1(alpha) ~ not(alphanum)  // Match a word not followed by letter or digit
```

### Look-Behind Patterns

Look-behind patterns allow checking what precedes the current position without consuming input. These are useful for context-sensitive matching.

```scala
// Positive look-behind - match if previous element satisfies predicate
lookBehind(c => c == 'a')  // Match if previous character is 'a'

// Negative look-behind - match if previous element does not satisfy predicate
notLookBehind(c => c.isDigit)  // Match if previous character is not a digit
```

Unlike regex look-behind which can check for patterns, these methods check only the single previous element against a predicate.

#### Examples

1. **Enforcing no double punctuation**:
```scala
// Ensure no period follows another period
'.' ~ notLookBehind(_ == '.')
```

2. **Context-sensitive matching**:
```scala
// Match digits only after a letter
alpha ~ lookBehind(_.isLetter) ~ digit
```

3. **Word boundaries**:
```scala
// Match a letter only if it's preceded by a space (word beginning)
ws1 ~ lookBehind(_.isWhitespace) ~ alpha ~ rep(alpha)
```

4. **Syntax highlighting helper**:
```scala
// Match an identifier character only if not preceded by another identifier character
// (useful for finding the start of identifiers)
alphanum ~ notLookBehind(c => c.isLetterOrDigit || c == '_')
```

Look-behind is particularly useful for implementing context-sensitive parsers, tokenizers, and lexers where the interpretation of a character depends on what came before it.

### Recursion and Non-Strict Evaluation

```scala
// For recursive pattern definitions
lazy val expr: Pattern = term ~ rep('+' ~ term)
lazy val term: Pattern = factor ~ rep('*' ~ factor)
lazy val factor: Pattern = digit | '(' ~ nonStrict(expr) ~ ')'

// nonStrict allows forward references in recursive definitions
nonStrict(() => pattern)
```

## Value Capture and Transformation

### Capturing Values

```scala
// Push a value onto the value stack
push(123)  // Push a literal value
push(new ListBuffer[Any])  // Push a container

// Get current input position
pointer  // Push current input position onto value stack

// Capture region between two points
capture(p)(action)  // Capture region matched by p and apply action to (start, end)

// Capture wrapped values
captureWrapped(p)  // Capture wrapped values matched by p
```

### Value Transformation

```scala
// Transform values on the stack
transform(2) { case Seq(a, b) => a.toString + b.toString }

// Action helpers for transformations with specific arities
action[Int](x => x * 2)  // Transform single value
action2[String, Int]((s, i) => s * i)  // Transform two values
action3[A, B, C]((a, b, c) => ...)  // Transform three values

// String capture
string(p)  // Capture matched text as string

// Example: capture identifier followed by colon
ident ~ ':' ~ action[String](name => Symbol(name))
```

## Backtracking Control

Recognizer provides fine-grained control over backtracking:

```scala
// Cut operator - prevents backtracking past this point
p ~ !! ~ q  // If p succeeds, commit to this path and never backtrack past q

// Fence - marker for backtracking control
fence ~ p  // Sets a checkpoint that cut can target

// Common pattern: ensure a pattern is not followed by something
val notFollowedBy = fence ~ (p ~ !! ~ failed | nop)
```

### How Backtracking Works

1. When an alternative `p | q` is encountered, a choice point is created
2. If `p` fails, the system backtracks to the choice point and tries `q`
3. The cut operator `!!` discards choice points, committing to the current path
4. Backtracking can be explicitly forced with `failed`

## String and Character Processing

`CharRecognizer` provides specialized patterns for character processing:

```scala
// Implicit conversion from string to pattern
"hello"  // Match the literal string "hello"

// Character classifications
alpha     // Match any letter
alphanum  // Match any letter or digit
digit     // Match any digit
digits    // Match one or more digits
ws        // Match zero or more whitespace
ws1       // Match one or more whitespace

// Special pattern builders
kw("if")   // Match the keyword "if" followed by non-alphanumeric and whitespace
sym("=>")  // Match the symbol "=>" followed by whitespace

// Identifier pattern
ident  // Match a Scala-like identifier
```

## Common Patterns and Idioms

### Collecting Repeated Matches

```scala
// Collect one or more elements into a list
rep1i(digit)  // Match one or more digits, return as List

// Collect with transformation
rep1a[Char](digit)(c => c.asDigit)  // Match digits, convert to numeric values

// Collect with arity-based transformation
rep1t(p, arity)(transform)  // Match repeated p, transform each match with arity operands
```

### Balanced Delimiters

```scala
// Recursive pattern for balanced parentheses
lazy val balanced: Pattern = rep(noneOf('(', ')') | '(' ~ nonStrict(balanced) ~ ')')

// Example from LinksImages.scala - balanced brackets
lazy val balancedText: Pattern = rep(noneOf('[', ']') | '[' ~ nonStrict(balancedText) ~ ']')
```

### Conditional Matching

```scala
// Test pattern - succeeds only if condition is met
test[Int](x => x > 0)  // Test that top of stack is positive
testValues(values => values.size > 2)  // Test on entire value stack
```

## Working Examples

### Basic Arithmetic Expression Parser

```scala
object Calculator extends Testing {
  // Forward declarations for recursive patterns
  lazy val expr: Pattern = term ~ rep(('+' | '-') ~ term ~ 
    action3[Int, Char, Int]((a, op, b) => op match {
      case '+' => a + b
      case '-' => a - b
    }))
  
  lazy val term: Pattern = factor ~ rep(('*' | '/') ~ factor ~ 
    action3[Int, Char, Int]((a, op, b) => op match {
      case '*' => a * b
      case '/' => a / b
    }))
  
  lazy val factor: Pattern = number ~ 
    action[String](s => s.toInt) | 
    '(' ~ ws ~ nonStrict(expr) ~ ')' ~ ws
  
  lazy val number: Pattern = string(rep1(digit)) ~ ws
  
  def evaluate(input: String): Option[Int] = 
    parse(input, expr).map(_._1.asInstanceOf[Some[Int]].value)
}
```

### Markdown Link Parser

Simplified version based on LinksImages.scala:

```scala
case class Link(text: String, url: String, title: Option[String])

object MarkdownParser extends Testing {
  // Pattern for balanced parentheses
  lazy val balancedParens: Pattern = rep(noneOf('(', ')') | '(' ~ nonStrict(balancedParens) ~ ')')
  
  // Pattern for balanced brackets
  lazy val balancedBrackets: Pattern = rep(noneOf('[', ']') | '[' ~ nonStrict(balancedBrackets) ~ ']')
  
  // Link pattern
  val linkPattern: Pattern =
    '[' ~ string(balancedBrackets) ~ ']' ~
    '(' ~ ws ~
    ('<' ~ string(rep(noneOf('>', '\n'))) ~ '>' | not('<') ~ string(balancedParens)) ~
    opti(
      ws1 ~ ('"' ~ string(rep(noneOf('"'))) ~ '"' | '\'' ~ string(rep(noneOf('\''))) ~ '\'' | 
            '(' ~ string(rep(noneOf(')'))) ~ ')')) ~ 
    ws ~ ')' ~ action3[String, String, Option[String]](Link.apply)
  
  def parseLink(input: String): Option[Link] =
    parse(input, linkPattern).flatMap(_._1.asInstanceOf[Option[Link]])
}
```

## API Reference

### Recognizer[W, E] Methods

| Method | Description |
|--------|-------------|
| `elem(e: E): Pattern` | Match a specific element |
| `clas(c: E => Boolean): Pattern` | Match any element satisfying predicate |
| `anyOf(es: E*): Pattern` | Match any element in the given set |
| `noneOf(es: E*): Pattern` | Match any element not in the given set |
| `any: Pattern` | Match any element |
| `nop: Pattern` | Always succeeds without consuming input |
| `failed: Pattern` | Always fails |
| `!!: Pattern` | Cut operator - disallows backtracking past this point |
| `fence: Pattern` | Marker for backtracking control |
| `not(p: Pattern): Pattern` | Negative lookahead - succeed only if p fails |
| `opt(p: Pattern): Pattern` | Optional pattern - succeed even if p fails |
| `rep(p: Pattern): Pattern` | Zero or more repetitions of p |
| `rep1(p: Pattern): Pattern` | One or more repetitions of p |
| `push(v: Any): Pattern` | Push value onto stack |
| `pointer: Pattern` | Push current input position onto stack |
| `capture(p: Pattern)(action: (I, I) => Any): Pattern` | Capture region matched by p and apply action |
| `transform(arity: Int)(f: Seq[Any] => Any): Pattern` | Transform top N values on stack |
| `action[A](f: A => Any): Pattern` | Transform top value on stack |
| `run(input: I, pat: Pattern): Option[(Option[Any], I, Runstate)]` | Run pattern on input |
| `runAll(input: I, pat: Pattern): List[(Option[Any], I)]` | Run pattern and return all possible matches |

### CharRecognizer[W] Properties

| Property | Description |
|----------|-------------|
| `alpha: Pattern` | Match any letter |
| `alphanum: Pattern` | Match any letter or digit |
| `digit: Pattern` | Match any digit |
| `digits: Pattern` | Match one or more digits |
| `whitespace: Pattern` | Match a whitespace character |
| `ws: Pattern` | Match zero or more whitespace |
| `ws1: Pattern` | Match one or more whitespace |
| `ident: Pattern` | Match an identifier |
| `number: Pattern` | Match a numeric literal |

### Pattern Operators

| Operator | Description |
|----------|-------------|
| `p ~ q` | Sequence - match p then q |
| `p \| q` | Alternative - match p or q |

### StringInput Methods

| Method | Description |
|--------|-------------|
| `eoi: Boolean` | Check if at end of input |
| `elem: Char` | Get current character |
| `wrapped: Char` | Get wrapped value (same as elem for StringInput) |
| `next: StringInput` | Get next input position |
| `rest: List[Char]` | Get remaining characters |