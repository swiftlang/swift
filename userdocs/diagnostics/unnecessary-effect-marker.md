# Unnecessary effect markers (UnnecessaryEffectMarker)

## Overview

These warnings are emitted when an effect marker — `try`, `await`, or `unsafe` — appears on an expression that contains no operation requiring it. The marker may have been added preemptively, the operation it was needed for may have been removed, or the resolved expression may not contain the operation that was expected to carry the effect.

The `UnnecessaryEffectMarker` group covers:
- `try` on an expression that calls no throwing functions
- `await` on an expression that performs no `async` operations
- `unsafe` on an expression that contains no unsafe operations
- `unsafe` on a `for`-`in` loop whose `Sequence` has a safe conformance

## Example

```swift
func greet(_ name: String) {
  print("Hello, \(name)")  // not throwing, not async, not unsafe
}

func caller() async throws {
  try greet("Ada")    // warning: no calls to throwing functions occur within 'try' expression
  await greet("Ada")  // warning: no 'async' operations occur within 'await' expression
  unsafe greet("Ada") // warning: no unsafe operations occur within 'unsafe' expression
}
```

## How to fix

Remove the marker:

```swift
func caller() async throws {
  greet("Ada")
}
```

