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

TBD

### Builtins

#### `_fastPath` and `_slowPath`

`_fastPath` returns its argument, wrapped in a Builtin.expect. This informs the optimizer that the vast majority of the time, the branch will be taken (i.e. the then branch is “hot”). The SIL optimizer may alter heuristics for anything dominated by the then branch. But the real performance impact comes from the fact that the SIL optimizer will consider anything dominated by the else branch to be infrequently executed (i.e. “cold”). This means that transformations that may increase code size have very conservative heuristics to keep the rarely executed code small.

The probabilities are passed through to LLVM as branch weight metadata, to leverage LLVM’s use of GCC style builtin_expect knowledge (e.g. for code layout and low-level inlining).

`_fastPath` probabilities are compounding, see the example below. For this reason, it can actually degrade performance in non-intuitive ways as it marks all other code (including subsequent `_fastPath`s) as being cold. Consider `_fastPath` as basically spraying the rest of the code with a Mr. Freeze-style ice gun.

`_slowPath` is the same as `_fastPath`, just with the branches swapped.

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


### Annotations

TBD

### Versioning and Compatibility

TBD

## Coding Standards

TBD


