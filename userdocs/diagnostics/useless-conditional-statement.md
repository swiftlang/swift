# Useless conditional statement (UselessConditionalStatement)

Warnings that identify conditional statements (`if`, `while`, and `guard`) whose conditions can never be false.

## Overview

When the condition of an `if`, `while`, or `guard` statement is statically known to always be true, the conditional structure is unnecessary. For `if` and `while`, the body always executes (and a `while` loop with no `break` would never terminate); for `guard`, the `else` branch is unreachable.

## Examples

```
let x = 0
if case _ = x { // warning: 'if' condition is always true
  // ...
}

while case _ = x { // warning: 'while' condition is always true
  break
}

guard case _ = x else { // warning: 'guard' condition is always true, body is unreachable
  fatalError()
}
```

In the examples above, the pattern matching conditions (`case _ = ...`) can never fail.

## See Also

- [If Statement][if-statement]
- [Guard Statement][guard-statement]
- [While Statement][while-statement]

[if-statement]: https://docs.swift.org/swift-book/documentation/the-swift-programming-language/statements/#If-Statement
[guard-statement]: https://docs.swift.org/swift-book/documentation/the-swift-programming-language/statements/#Guard-Statement
[while-statement]: https://docs.swift.org/swift-book/documentation/the-swift-programming-language/statements/#While-Statement
