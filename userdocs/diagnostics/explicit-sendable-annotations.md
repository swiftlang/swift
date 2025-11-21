# Explicit Sendable annotations on public type declarations

If a public type doesn't have an explicit Sendable or non-Sendable annotation it is sometimes hard to discern whether that is intentional or not, especially if a type could be Sendable.

## Overview

The Swift compiler would emit a warning if a public type has none of the following:

  - A conformance to `Sendable` protocol;
  - An unavailable conformance to `Sendable` protocol;
  - `~Sendable` conformance to suppress the inference.

Let's consider a simple public type without any Senable annotations:

```
public struct S {
    let x: Int
}
```

When compiling with `-Wwarning ExplicitSendable` the following warning is going to be produced by the Swift compiler:

```
1 | public struct S {
  |               |- warning: public struct 'S' does not specify whether it is 'Sendable' or not [#ExplicitSendable]
  |               |- note: consider making struct 'S' conform to the 'Sendable' protocol
  |               `- note: make struct 'S' explicitly non-Sendable to suppress this warning
2 |     let x: Int
3 | }
```
