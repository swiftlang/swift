# Standard Library Programmers Manual

This is meant to be a guide to people working on the standard library. It covers coding standards, code organization, best practices, internal annotations, and provides a guide to standard library internals. This document is inspired by LLVM's excellent [programmer's manual](http://llvm.org/docs/ProgrammersManual.html) and [coding standards](http://llvm.org/docs/CodingStandards.html).

TODO: Should this subsume or link to [StdlibRationales.rst](https://github.com/apple/swift/blob/master/docs/StdlibRationales.rst)?

TODO: Should this subsume or link to [AccessControlInStdlib.rst](https://github.com/apple/swift/blob/master/docs/AccessControlInStdlib.rst)


## (Meta): List of wants and TODOs for this guide

1. Library Organization
    1. What files are where
        1. Brief about CMakeLists
        1. Brief about GroupInfo.json
    1. What tests are where
        1. Furthermore, should there be a split between whitebox tests and blackbox tests?
    1. What benchmarks are where
        1. Furthermore, should there be benchmarks, microbenchmarks, and nanobenchmarks (aka whitebox microbenchmarks)?
    1. What SPIs exist, where, and who uses them
    1. Explain test/Prototypes, and how to use that for rapid (relatively speaking) prototyping
1. Library Concepts
    1. Protocol hierarchy
        1. Customization hooks
    1. Use of classes, COW implementation, buffers, etc
    1. Compatibility, `@available`, etc.
    1. Resilience, ABI stability, `@inlinable`, `@usableFromInline`, etc
    1. Strings and ICU
    1. Lifetimes
        1. withExtendedLifetime, withUnsafe...,
    1. Shims and stubs
1. Coding Standards
    1. High level concerns
    1. Best practices
    1. Formatting
1. Internals
    1. `@inline(__always)` and `@inline(never)`
    1. `@semantics(...)`
    1. Builtins
        1. Builtin.addressof, _isUnique, _isUniqueOrPinned, etc
1. Dirty hacks
    1. Why all the underscores and extra protocols?
    1. How does the `...` ranges work?
1. Frequently Encountered Issues


## Internals

#### Unwrapping Optionals

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

A call to `_fixLifetime` is considered a use of its argument, meaning that the argument is guaranteed to live at least up until the call. It is otherwise a nop. This is useful for guaranteeing the lifetime of a value while inspecting its physical layout. Without a call to `_fixLifetime`, the last formal use may occur while the value's bits are still being munged.

*Example:*

```swift
var x = ...
defer { _fixLifetime(x) } // Guarantee at least lexical lifetime for x
let theBits = unsafeBitCast(&x, ...)
... // use of theBits in ways that may outlive x if it weren't for the _fixLifetime call
```

### Annotations

#### `@_transparent`

Should only be used if necessary. This has the effect of forcing inlining to occur before any dataflow analyses take place. Unless you specifically need this behavior, use `@_inline(__always)` or some other mechanism. Its primary purpose is to force the compiler's static checks to peer into the body for diagnostic purposes.

Use of this attribute imposes limitations on what can be in the body. For more details, refer to the [documentation](https://github.com/apple/swift/blob/master/docs/TransparentAttr.rst).

#### `@unsafe_no_objc_tagged_pointer`

This is currently used in the standard library as an additional annotation applied to @objc protocols signifying that any objects which conform to this protocol are not tagged. This means that (on Darwin platforms) such references, unlike AnyObject, have spare bits available from things like restricted memory spaces or alignment.

#### `@_silgen_name`

This attribute specifies the name that a declaration will have at link time. It is used for two purposes, the second of which is currently considered bad practice and should be replaced with shims:

1. To specify the symbol name of a Swift function so that it can be called from Swift-aware C. Such functions have bodies.
2. To provide a Swift declaration which really represents a C declaration. Such functions do not have bodies.

##### Using `@_silgen_name` to call Swift from Swift-aware C

Rather than hard-code Swift mangling into C code, `@_silgen_name` is used to provide a stable and known symbol name for linking. Note that C code still must understand and use the Swift calling convention (available in swift-clang) for such Swift functions (if they use Swift's CC). Example:

```swift
@_silgen_name("_destroyTLS")
internal func _destroyTLS(_ ptr: UnsafeMutableRawPointer?) {
  // ... implementation ...
}
```

```C++
SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERNAL
void _destroyTLS(void *);

// ... C code can now call _destroyTLS on a void * ...
```

##### Using `@_silgen_name` to call C from Swift

The standard library cannot import the Darwin module (much less an ICU module), yet it needs access to these C functions that it otherwise wouldn't have a decl for. For that, we use shims. But, `@_silgen_name` can also be used on a body-less Swift function declaration to denote that it's an external C function whose symbol will be available at link time, even if not available at compile time. This usage is discouraged.


### Internal structures

#### `_FixedArray`

The standard library has internal fixed size arrays of some limited sizes. This provides fast random access into contiguous (usually stack-allocated) memory. These are metaprogrammed based on size, so if you need a new size not currently defined, add it to the `sizes` gyb variable. See [FixedArray.swift.gyb](https://github.com/apple/swift/blob/master/stdlib/public/core/FixedArray.swift.gyb) for implementation.

#### Thread Local Storage

The standard library utilizes thread local storage (TLS) to cache expensive computations or operations in a thread-safe fashion. This is currently used for tracking some ICU state for Strings. Adding new things to this struct is a little more involved, as Swift lacks some of the features required for it to be expressed elegantly (e.g. move-only structs):

1. Add the new member to `_ThreadLocalStorage` and a static `getMyNewMember` method to access it. `getMyNewMember` should be implemented using `getPointer`.
2. If the member is not trivially initializable, update `_initializeThreadLocalStorage` and `_ThreadLocalStorage.init`.
3. If the field is not trivially destructable, update `_destroyTLS` to properly destroy the value.

See [ThreadLocalStorage.swift](https://github.com/apple/swift/blob/master/stdlib/public/core/ThreadLocalStorage.swift) for more details.

## Productivity Hacks

### Be a Ninja

To *be* a productivity ninja, one must *use* `ninja`. `ninja` can be invoked inside the swift build directory, e.g. `<path>/build/Ninja-ReleaseAssert/swift-macosx-x86_64/`. Running `ninja` (which is equivalent to `ninja all`) will build the local swift, stdlib and overlays. It doesn’t necessarily build all the testing infrastructure, benchmarks, etc.

`ninja -t targets` gives a list of all possible targets to pass to ninja. This is useful for grepping.

For this example, we will figure out how to quickly iterate on a change to the standard library to fix 32-bit build errors while building on a 64-bit host, suppressing warnings along the way.

`ninja -t targets | grep stdlib | grep i386` will output many targets, but at the bottom we see `swift-stdlib-iphonesimulator-i386`, which looks like a good first step. This target will just build i386 parts and not waste our time also building the 64-bit stdlib, overlays, etc.

Going further, ninja can spawn a web browser for you to navigate dependencies and rules. `ninja -t browse swift-stdlib-iphonesimulator-i386`  will open a webpage with hyperlinks for all related targets. “target is built using” lists all this target’s dependencies, while “dependent edges build” list all the targets that depend directly on this.

Clicking around a little bit, we can find `lib/swift/iphonesimulator/i386/libswiftCore.dylib` as a commonly-depended-upon target. This will perform just what is needed to compile the standard library for i386 and nothing else.

Going further, for various reasons the standard library has lots of warnings. This is actively being addressed, but fixing all of them may require language features, etc. In the mean time, let’s suppress warnings in our build so that we just see the errors. `ninja -nv lib/swift/iphonesimulator/i386/libswiftCore.dylib` will show us the actual commands ninja will issue to build the i386 stdlib. (You’ll notice that an incremental build here is merely 3 commands as opposed to ~150 for `swift-stdlib-iphonesimulator-i386`).

Copy the invocation that has  ` -o <build-path>/swift-macosx-x86_64/stdlib/public/core/iphonesimulator/i386/Swift.o`, so that we can perform the actual call to swiftc ourselves. Tack on `-suppress-warnings` at the end, and now we have the command to just build `Swift.o` for i386 while only displaying the actual errors.
