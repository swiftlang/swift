# Unavailable overrides (UnavailableOverride)

Diagnostics that identify overrides that are marked unavailable even though the declarations they override are available.

## Overview

A declaration that overrides an available declaration in a superclass must not be unconditionally unavailable. Marking an override unavailable with the `@available` attribute does not remove it from the subclass, so calls made through a reference to the superclass still dispatch to the override at runtime. For example:

```swift
class Shape {
  func draw() { /* ... */ }
}

class Circle: Shape {
  @available(*, unavailable) // warning: cannot override 'draw' with a declaration that is marked unavailable
  override func draw() { /* ... */ }
}

let shape: Shape = Circle()
shape.draw() // Calls 'Circle.draw()'
```

The body of an unavailable declaration allows references to other declarations that are unavailable under the same conditions. Being able to reach such a declaration dynamically could break runtime guarantees that must be upheld at compile time.

## How to fix

- Remove the `@available` attribute from the override, so that the override is available whenever the declaration it overrides is available. If the override must not be called, implement it with a body that reports the misuse, such as a call to `fatalError()`.

- If the subclass itself must not be used, mark the entire subclass unavailable instead of its individual overrides:

  ```swift
  @available(*, unavailable)
  class Circle: Shape {
    override func draw() { /* ... */ }
  }
  ```

- If clients should stop calling the override directly, mark it `deprecated` or `obsoleted` instead of `unavailable`.
