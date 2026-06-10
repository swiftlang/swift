# Default Literal Types

Type aliases representing the concrete type that a literal takes when no other type
information is provided.

## Overview

This example declares the `numberOfCookies` constant, using an integer literal to
express its value:

```swift
let numberOfCookies = 5
// type(of: numberOfCookies) == Int.self
```

When a literal expression is written with no type information, Swift uses these type
aliases to determine what type to use for the expression. In this case, the `numberOfCookies`
constant has the default type for an integer literal, `Int`, as designated by the
`IntegerLiteralType` type alias.

## Topics

### Basic Values

- ``Swift/BooleanLiteralType``
- ``Swift/IntegerLiteralType``
- ``Swift/FloatLiteralType``

### Strings and Text

- ``Swift/StringLiteralType``
- ``Swift/ExtendedGraphemeClusterType``
- ``Swift/UnicodeScalarType``
