# Type Inference in Complex Expressions
For some expressions, the Swift compiler is unable to infer types in reasonable time. This can occur for a number of reasons, but some common ones include:

- The expression makes use of many overloaded operators and/or functions.
- The expression contains literals which can be used as values of many different types (for example, integer or floating point literals).
- The expression includes a collection literal with a large number of elements.

These types of expressions can take a long time to typecheck because the compiler must consider and eliminate a larger number of possible solutions when assigning types to subexpressions.

When this error occurs, possible solutions include:

- Separating the expression into multiple subexpressions by storing intermediate values as `let` constants. These simpler subexpressions can have their types inferred more quickly.

```swift
// error: the compiler is unable to type-check this expression in reasonable time
let approximateESquared = 1 + 2 + 4.0/2 + 8.0/6 + 16.0/24 + 32.0/120

// OK
let x = 1 + 2 + 4.0/2
let y = 8.0/6 + 16.0/24 + 32.0/120
let approximateESquared = x + y
```

- Providing manual type annotations for parts of the expression. This reduces the number of possible solutions the compiler must consider.

```swift
// error: the compiler is unable to type-check this expression in reasonable time
let approximateESquared = 1 + 2 + 4.0/2 + 8.0/6 + 16.0/24 + 32.0/120

// OK
let approximateESquared = 1 + 2 + (4.0/2 as Double) + (8.0/6 as Double) + (16.0/24 as Double) + (32.0/120 as Double)

// Also OK
let approximateESquared = 1 + 2 + Double(4.0/2) + Double(8.0/6) + Double(16.0/24) + Double(32.0/120)
```

- Reducing the number of overloaded declarations. New overloads of common operators like `+` or global functions like `max` may lead to slower type inference in some contexts.

When you encounter an expression the Swift compiler is unable to typecheck in reasonable time, please consider filing a bug at [bugs.swift.org](http://bugs.swift.org) with example code that reproduces the issue.