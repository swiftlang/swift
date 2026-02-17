# @_lifetime annotation

## Introduction

`@_lifetime` annotations are now available under `-enable-experimental-features Lifetimes`. The feature proposal is documented in [PR: Lifetime dependencies #2750](https://github.com/swiftlang/swift-evolution/pull/2750).

To summarize the basic syntax: functions require a `@_lifetime` annotation when they return a non-Escapable type, either as the function result, or as an `inout` parameter. The annotation syntax pattern is `@_lifetime(target: <scope> source)` where `target` is a function output and `source` is an input. If `target:` is omitted, then it is assumed to be the function's result.

`<scope> ::= [borrow|&|copy]`

`borrow` creates a new borrow scope that guarantees exclusive read access to the caller's `source` argument over all uses of `target`.

`&` creates a new borrow scope that guarantees exclusive write access to the caller's `source` argument over all uses of `target`.

`copy` copies the lifetime constraints on the caller's `source` argument to `target`.

The `@lifetime` annotation is enforced both in the body of the function and at each call site. For both `borrow` and `&` scoped dependencies, the function's implementation guarantees that `target` is valid as long as `source` is alive, and each caller of the function guarantees that `source` will outlive `target`. For `copy` dependencies, the function's implementation guarantees that all constraints on `target` are copied from `source`, and the caller propagates all lifetime constraints on `source` to all uses of `target`.

## Default lifetimes

The Swift 6.2 compiler provided default `@_lifetime` behavior whenever it can do so without ambiguity. Often, despite ambiguity, an obvious default exists, but we wanted to introduce defaults slowly after developers have enough experience to inform discussion about them. This document tracks the current state of the implementation as it progresses from the original 6.2 implementation. Corresponding tests are in `test/Sema/lifetime_depend_infer.swift`; searching for "DEFAULT:" highlights the rules defined below...

### Same-type default lifetime

Given a function declaration:

`func foo(..., a: A, ...) -> R { ... }`

Where `R: ~Escapable`, `A == R`, and `a` is not an `inout` parameter, default to `@_lifetime(copy a)`.
For non-mutating methods, the same rule applies to implicit `Self` parameter.

This handles the obvious cases in which both the parameter and result are `~Escapable`. For example:

```swift
extension Span {
  /* DEFAULT: @_lifetime(copy self) */
  func extracting(droppingLast k: Int) -> Self { ... }
}
```

#### Generic same-type default lifetime

The same-type default lifetime rule described above is convenient for nominal types but essential for generic type parameters.

Given a generic function declaration:

`func foo<A, R>(..., a: A, ...) -> R { ... }`

The same-type default lifetime rule applies to the types in the function declaration's generic context just as it did for nominal types in the previous example. So, again, if type resolution determines `R: ~Escapable` and `A == R`, then `@_lifetime(copy a)` will be default.

Unlike nominal types, the programmer is not allowed to explicitly declare a lifetime dependency, `@_lifetime(copy
a)`, unless the argument and result types are equivalent (`A == R`). Copying a lifetime dependency from one value to another requires that both values are non-Escapable. Generic types are conditionally non-Escapable (their lifetime dependencies are type-erased), so type equivalence is the only way to ensure that both values are non-Escapable under the same conditions.

Here we see how same-type lifetime requirement applies to type substitution and associated types:

```swift
protocol P {
  associatedtype T: ~Escapable
}

protocol Q {
  associatedtype U: ~Escapable
}

struct S<A: P, B: Q> {
  /* OK: @_lifetime(copy a) is valid and default */
  func foo(a: A.T) -> B.U where A.T == B.U
}
```

Note that lifetime dependencies are resolved at function declaration time, which determines the function's type. The generic context at the point of function invocation is not considered. For example, the following declaration of `foo` is invalid, because it's argument and results types don't match at the point of declaration, even though the argument and result do have the same type when invoked inside `bar`:

```swift
struct S<T: ~Escapable, U: ~Escapable> {
  static func foo(a: T) -> U // ERROR: missing lifetime dependency
}

/* OK: @_lifetime(copy a) is valid and default */
func bar<T: ~Escapable>(a: T) -> T {
  S<T, T>.foo(a: a) // The same-type rule is satisfied in this context, but 'foo's declaration is invalid.
}
```

### `inout` parameter default rule

The following `inout` parameter default rule directly follows from the general same-type default rule above. We descrive it as a separate rule here only because the behavior is important and would otherwise be nonobvious:

- Default to `@_lifetime(a: copy a)` for all `inout` parameters where `a` is  `~Escapable`.

- Default to `@_lifetime(self: copy self)` on `mutating` methods where `self` is `~Escapable`.

#### Examples

```swift
struct A: Escapable {
  let obj: AnyObject // ~BitwiseCopyable
}
struct NE: ~Escapable {...}

/* DEFAULT: @_lifetime(a: copy a) */
func inoutNEParam_void(_: inout NE) -> ()

/* DEFAULT: @_lifetime(a: copy a) */
/* DEFAULT: @_lifetime(b: copy b) */
func inoutNEParam_inoutNEParam_void(a: inout NE, b: inout NE) -> ()

/* DEFAULT: @_lifetime(ne: copy ne) */
@_lifetime(&ne)
func inoutNEParam_NEResult(ne: inout NE) -> NE

extension A /* Self: Escapable */ {
  /* DEFAULT: @_lifetime(ne: copy NE) */
  func inoutNEParam_void(a: inout ) -> ()

  /* DEFAULT: @_lifetime(ne: copy NE) */
  mutating func mutating_inoutNEParam_void() -> ()

  /* DEFAULT: @_lifetime(ne: copy NE) */
  @_lifetime(&self)
  func inoutNEParam_NEResult(ne: inout NE) -> NE
}

extension NE /* Self: ~Escapable */ {
  /* DEFAULT: @_lifetime(self: copy self) */
  mutating func mutating_noParam_void() -> ()

  /* DEFAULT: @_lifetime(self: copy self) */
  mutating func mutating_oneParam_void(_: NE) -> ()

  /* DEFAULT: @_lifetime(self: copy self) */
  /* DEFAULT: @_lifetime(ne: copy ne) */
  mutating func mutating_inoutParam_void(ne: inout NE) -> ()

  /* DEFAULT: @_lifetime(self: copy self) */
  @_lifetime(&self)
  mutating func mutating_noParam_NEResult() -> NE
}
```

### Single parameter default rule

Given a function or method that returns a non-Escapable result, if that result's dependency does not have a same-type default:

- Default to `@_lifetime(<scope> a)` for a `~Escapable` result on functions with a single parameter `a`.

- Default to `@_lifetime(<scope> self)` for a `~Escapable` result on methods with no parameters.

| Type of parameter | default                        |
| (`a` or `self`)   | lifetime dependency            |
| ----------------- | ------------------------------ |
| `Escapable`       | `@_lifetime(borrow param)`[^1] |
| `inout Escapable` | `@_lifetime(&param)`[^1]       |
| `~Escapable`      | none[^2]                       |

[^1]: When the parameter is `BitwiseCopyable`, such as an integer or unsafe pointer, the single parameter default rule applies to function parameters but not to the implicit `self` parameter. Depending on a `BitwiseCopyable` value is a convenience for APIs that construct span-like values from an `UnsafePointer` passed as an argument. This creates a dependency on a local copy of the pointer variable with subtle semantics. User-defined `BitwiseCopyable` structs should generally avoid such subtle lifetime dependencies. If needed, the author of the data type should explicitly opt into them.

[^2]: When the single parameter is also `~Escapable`, the result must depend on it, but the dependency may either be scoped (`borrow` or `&`) or it may be copied (`copy`). `copy` is the obvious choice when the parameter and result are the same type, but it is not always correct. Furthermore, a lifetime dependency can only be copied from a generic type when result as the same generic type. This case is therefore handled by same-type default lifetime (discussed below) rather than as a default `@_lifetime` rule.

Examples:

```swift
struct A: Escapable {
  let obj: AnyObject // ~BitwiseCopyable
}
struct NE: ~Escapable {...}

/* DEFAULT: @_lifetime(borrow a) */
func oneParam_NEResult(a: A) -> NE

/* DEFAULT: @_lifetime(&a) */
func oneInoutParam_NEResult(a: inout A) -> NE

extension A /* Self: Escapable */ {
  /* DEFAULT: @_lifetime(borrow self) */
  func noParam_NEResult() -> NE

  /* DEFAULT: @_lifetime(&self) */
  mutating func mutating_noParam_NEResult() -> NE
}
```

### Implicit initializer and setter defaults

An implicit setter of a `~Escapable` stored property defaults to `@_lifetime(self: copy self, copy newValue)`. This is always correct because the setter simply assigns the stored property to the newValue. Assigning a `~Escapable` variable copies the lifetime dependency.

Similarly, an implicit initializer of a non-Escapable struct defaults to `@_lifetime(self: copy arg)` if all of the initializer arguments are `~Escapable`. This is equivalent to assigning each `~Escapable` stored property. If, however, any initializer arguments are `Escapable`, then no default lifetime is provided unless it is the sole argument, in which case the single parameter rule applies.

## Function Type Lifetimes

Function types can also have lifetime dependencies. This makes it possible to pass a callback function parameter that returns a non-Escapable type.
The annotation syntax is the same as above, and the default lifetime inference rules for non-member functions apply.
Support for lifetime dependencies on captured values is not yet implemented, so methods and certain closures (see below) cannot be passed.

Examples:

```swift
struct NE: ~Escapable {}

func processNEs(ne0: NE, ne1: NE) -> NE { ne0 }

/* DEFAULT: @_lifetime(copy ne0, copy ne1) */
func takeProcessor(ne0: NE, ne1: NE,
                   /* DEFAULT: @_lifetime(copy $0, copy $1). */
                   fn: (NE, NE) -> NE) -> NE {
    return fn(ne0, ne1)
}

_ = takeProcessor(ne0: NE(), ne1: NE(), fn: processNEs)
```

Note that function types cannot have argument labels, so you must use internal labels in lifetime annotations:

```swift
@_lifetime(copy ne0)
func processNEs2(ne0: NE, ne1: NE) -> NE { ne0 }

func takeProcessor2(ne0: NE, ne1: NE,
                    fn: @_lifetime(copy ne0) (_ ne0: NE, NE) -> NE) -> NE {
    return fn(ne0, ne1)
}
```

Currently, there is no way to express lifetime dependencies on the captured context.
Closures can use the captured context, but cannot return or write to captured `~Escapable` values.

```swift
func takePicker(/* DEFAULT: @_lifetime(copy ne0, copy ne1) */
                picker: (NE, NE) -> NE) {
    let x = NE()
    let y = NE()
    _ = picker(x, y)
}
func predicate(ne: NE) -> Bool { ... }

// OK, only parameters are used
takePicker { ne0, ne1 in ne0 }

// Error: Captured ~Escapable variable ne2 escapes.
let ne2 = NE()
takePicker { ne0, ne1 in ne2 }

// OK, ne3 is captured but it doesn't escape.
let ne3 = NE()
takePicker { ne0, ne1 in
  if predicate(ne: ne3) { return ne0 } else { return ne1 }
}

```
