# Variable never mutated (VariableNeverMutated)

Warnings that identify local `var` bindings which are never mutated after
initialization and could instead be declared with `let`.

## Overview

When a variable is introduced with `var` but never reassigned or otherwise
mutated, it can be declared with `let` instead to explicitly indicate that the
variable is constant.

## Example

```swift
func swap(_ a: inout Int, _ b: inout Int) {
  var temp = a // warning: variable 'temp' was never mutated; consider changing to 'let' constant
  a = b
  b = temp
}
```

The fix is to change `var` to `let`:

```swift
func swap(_ a: inout Int, _ b: inout Int) {
  let temp = a
  a = b
  b = temp
}
```

## Sub-groups

The `VariableNeverMutated` group contains the following sub-group, which can
also be enabled or controlled independently:

- <doc:weak-mutability>: Warns when a `weak var` binding is never mutated and
  could be a `weak let` instead.
