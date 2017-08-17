# Standard Library Programmers Manual

This is meant to be a guide to people working on the standard library. It covers coding standards, code organization, best practices, internal annotations, and provides a guide to standard library internals. This document is inspired by LLVM's excellent [programmer's manual](http://llvm.org/docs/ProgrammersManual.html) and [coding standards](http://llvm.org/docs/CodingStandards.html).

TBD: Should this subsume [StdlibRationales.rst](https://github.com/apple/swift/blob/master/docs/StdlibRationales.rst)?

## Library Organization

TBD

### Stubs

TBD

## Protocols and Classes

TBD

### Customization Hooks

TBD

## Internals

### Internal Functionality and SPI

#### Optionals

Optionals can be unwrapped with `!`, which triggers a trap on nil. Alternatively, they can be `.unsafelyUnwrapped()`, which will check and trap in debug builds of user code. Internal to the standard library is `._unsafelyUnwrappedUnchecked()` which will only check and trap in debug builds of the standard library itself. These correspond directly with `_precondition`, `_debugPrecondition`, and `_sanityCheck`. See [that section](#precondition) for details.


### Builtins

#### `_fastPath` and `_slowPath` (also, `_branchHint`)

`_fastPath` returns its argument, wrapped in a Builtin.expect. This informs the optimizer that the vast majority of the time, the branch will be taken (i.e. the then branch is “hot”). The SIL optimizer may alter heuristics for anything dominated by the then branch. But the real performance impact comes from the fact that the SIL optimizer will consider anything dominated by the else branch to be infrequently executed (i.e. “cold”). This means that transformations that may increase code size have very conservative heuristics to keep the rarely executed code small.

The probabilities are passed through to LLVM as branch weight metadata, to leverage LLVM’s use of GCC style builtin_expect knowledge (e.g. for code layout and low-level inlining).

`_fastPath` probabilities are compounding, see the example below. For this reason, it can actually degrade performance in non-intuitive ways as it marks all other code (including subsequent `_fastPath`s) as being cold. Consider `_fastPath` as basically spraying the rest of the code with a Mr. Freeze-style ice gun.

`_slowPath` is the same as `_fastPath`, just with the branches swapped. Both are just wrappers around `_branchHint`, which is otherwise never called directly.

*Example:*

```swift
if _fastPath(...) {
  // 90% of the time we execute this: aggressive inlining
  ...
  return
}
// 10% of the time we execute this: very conservative inlining
...
if _fastPath(...) {
  // 9% of the time we execute this: very conservative inlining
	...
	return
}

// 1% of the time we execute this: very conservative inlining
...
return
```

*NOTE: these are due for a rename and possibly a redesign. They conflate multiple notions that don’t match the average standard library programmer’s intuition.*


#### `_onFastPath` 

This should be rarely used. It informs the SIL optimizer that any code dominated by it should be treated as the innermost loop of a performance critical section of code. It cranks optimizer heuristics to 11. Injudicious use of this will degrade performance and bloat binary size.


#### <a name="precondition"></a>`_precondition`, `_debugPrecondition`, and `_sanityCheck`

These three functions are assertions that will trigger a run time trap if violated.

* `_precondition` executes in all build configurations. Use this for invariant enforcement in all user code build configurations
* `_debugPrecondition` will execute when **user code** is built with assertions enabled. Use this for invariant enforcement that's useful while debugging, but might be prohibitively expensive when user code is configured without assertions.
* `_sanityCheck` will execute when **standard library code** is built with assertions enabled. Use this for internal only invariant checks that useful for debugging the standard library itself.

#### `_fixLifetime`

A call to `_fixLifetime` is considered a use of its argument, meaning that the argument is guaranteed live at least up until the call. It is otherwise a nop. This is useful for guaranteeing the lifetime of a value while inspecting its physical layout. Without a call to `_fixLifetime`, the last formal use may occur while the value's bits are still being munged.

*Example:*

```swift
var x = ...
defer { _fixLifetime(x) } // Guarantee at least lexical lifetime for x
let theBits = unsafeBitCast(&x, ...)
... // use of theBits in ways that may outlive x if it weren't for the _fixLifetime call
```


### Annotations

#### `@_versioned`

TBD

#### `@_inlineable`

TBD

#### `@inline(__always)` and `@inline(never)`

TBD

#### `@_semantics(...)`

TBD

#### `@_transparent`

Should only be used if necessary. This has the effect of forcing inlining to occur before any dataflow analysis take place. Unless you specifically need this behavior, use `@_inline(__always)` or some other mechanism. Its primary purpose is to force the compiler's static checks to peer into the body for diagnostic purposes.

Use of this attribute imposes limitations on what can be in the body. For more details, refer to the [documentation](https://github.com/apple/swift/blob/master/docs/TransparentAttr.rst).

### Versioning and Compatibility

#### `@available`

TBD

#### `@_silgen_name`

TBD

## Coding Standards

TBD


