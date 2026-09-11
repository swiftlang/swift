# Dynamic cast (DynamicCast)

Warnings about dynamic casts whose result is known at compile time or that the
Swift runtime cannot perform.

## Overview

Swift uses the `is`, `as?`, and `as!` operators to check or cast a value's type
at runtime. The compiler warns when type information proves that an `is` check
is always true, or when the runtime does not support the requested conversion
and the check or cast will always fail.

For example, checking a concrete value against a protocol that its type is
known to conform to is unnecessary:

```swift
protocol P {}
struct S: P {}

let value = S()
if value is any P { // warning: 'is' test is always true
  // ...
}
```

Remove an always-true check when possible. For an unsupported runtime cast,
change the source or destination type, or use a statically valid `as` coercion
when the diagnostic suggests one.

## See Also

- [Type Casting][type-casting]

[type-casting]: https://docs.swift.org/swift-book/documentation/the-swift-programming-language/typecasting
