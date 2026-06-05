# Unnecessary unsafe (UnnecessaryUnsafe)

## Overview

This warning is emitted when an `unsafe` marker appears on an expression that contains no unsafe operations. The marker may have been added preemptively or the unsafe operation it was needed for may no longer be considered unsafe.

This group is a subgroup of <doc:unnecessary-effect-marker>.

## Example

Some APIs with 'unsafe' in their names are actually annotated with `@safe`. For example, invoking `Array.withUnsafeBytes(_:)` does not require an `unsafe` marker when `-strict-memory-safety` is specified:

```swift
func process(_ values: [UInt8]) {
  // warning: no unsafe operations occur within 'unsafe' expression
  unsafe values.withUnsafeBytes { _ in
    print("Processing \(values.count) bytes")
  }
}
```

The call to `withUnsafeBytes(_:)` by itself is safe and the closure body performs no unsafe operations, so the outer `unsafe` is unnecessary. The unsafe operation would be using the buffer's pointer inside the closure, which can be acknowledged closer to the source of the unsafety.

## How to fix

Remove the `unsafe` marker from the outer expression. If a use inside the closure is the actually-unsafe operation, mark only that use:

```swift
func process(_ values: [UInt8]) {
  values.withUnsafeBytes { buffer in
    let first = unsafe buffer.first ?? 0
    print("First byte: \(first)")
  }
}
```

## See Also

- <doc:unnecessary-effect-marker>
- <doc:strict-memory-safety>
