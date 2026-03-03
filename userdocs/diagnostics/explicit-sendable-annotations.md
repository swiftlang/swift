# Explicit `Sendable` annotations on public type declarations (ExplicitSendable)

Adds a warning for any public types without a `Sendable` annotation.

## Overview

When enabled, the compiler will emit a warning if a public type has none of the following:
  - A conformance to `Sendable` protocol;
  - An unavailable conformance to `Sendable` protocol;
  - `~Sendable` conformance to suppress the inference.

For example, given a simple public type:
```
public struct S {
    let x: Int
}
```

As it has no `Sendable` annotations, this diagnostic group will add the following warning:
```
1 | public struct S {
  |               |- warning: public struct 'S' does not specify whether it is 'Sendable' or not [#ExplicitSendable]
  |               |- note: consider making struct 'S' conform to the 'Sendable' protocol
  |               `- note: make struct 'S' explicitly non-Sendable to suppress this warning
2 |     let x: Int
3 | }
```
