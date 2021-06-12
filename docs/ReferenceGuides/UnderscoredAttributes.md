# Underscored Attributes Reference

**WARNING:** This information is provided primarily for compiler and standard
library developers. Usage of these attributes outside of the Swift monorepo
is STRONGLY DISCOURAGED.

The Swift reference has a chapter discussing [stable attributes][Attributes].
This document is intended to serve as a counter-part describing underscored
attributes, whose semantics are subject to change and most likely need to
go through the Swift evolution process before being stabilized.

[Attributes]: https://docs.swift.org/swift-book/ReferenceManual/Attributes.html

The attributes are organized in alphabetical order.

## `@_alignment`

## `@_alwaysEmitIntoClient`

Forces the body of a function to be emitted into client code. Note that this is
distinct from `@inline(__always)`; it doesn't force inlining at call-sites, it
only means that the implementation is compiled into the module which uses the
code.

This means that `@_alwaysEmitIntoClient` definitions are _not_ part of the
defining module's ABI, so changing the implementation at a later stage
does not break ABI. (TODO: Does it matter if the `@_alwaysEmitIntoClient`
definition is used in the defining module, say in an `@inlinable function?)

Most notably, default argument expressions are implicitly
`@_alwaysEmitIntoClient`, which means that adding
a default argument to a function which did not have one previously
does not break ABI.

## `@_borrowed`

## `@_cdecl`

Similar to `@_silgen_name` but uses the C calling convention.
This attribute doesn't have very well-defined semantics.

Type bridging is not done, so the parameter and return types should
correspond directly to types accessible in C.
In most cases, it is preferable to define a static method on an `@objc`
class instead of using `@_cdecl`.

For potential ideas on stabilization, see
[Formalizing `@cdecl`](https://forums.swift.org/t/formalizing-cdecl/40677).

## `@_disfavoredOverload`

Marks a declaration that the type checker should try to avoid using. When the
expression type checker is considering overloads, it will prefer a solution
with fewer `@_disfavoredOverload` declarations over one with more of them.

Historically, it was designed to help with `ExpressibleByXYZLiteral` types.
The type checker strongly prefers to give literals their default type
(e.g. `Int` for `ExpressibleByIntegerLiteral`,
`String` for `ExpressibleByStringLiteral`, etc.).
If an API should prefer some other type, but accept the default too,
marking the declaration taking the default type with `@_disfavoredOverload`
gives the desired behavior:

```swift
extension LocalizedStringKey: ExpressibleByStringLiteral { ... }

extension Text {
  // We want `Text("foo")` to use this initializer:
  init(_ key: LocalizedStringKey) { ... }

  // But without @_disfavoredOverload, it would use this one instead,
  // because that lets it give the literal its default type:
  @_disfavoredOverload init<S: StringProtocol>(_ str: S) { ... }
}
```

`@_disfavoredOverload` is a stop-gap design; eventually the type-checker
behavior should be fixed and the attribute should be removed/made into a no-op.

Its main use case is to work around clearly undesirable overload resolution,
not to arbitrarily manipulate overload priority.

## `@_dynamicReplacement(for:)`

Marks a function as the dynamic replacement for another `dynamic` function.
This is similar to method swizzling in other languages such as Objective-C,
except that the replacement happens at program start (or loading a shared
library), instead of at an arbitrary point in time.

For more details, see [Dynamic method replacement](https://forums.swift.org/t/dynamic-method-replacement/16619).

## `@_distributedActorIndependent`

## `@_effects`

## `@_exported`

Use to mark an imported module to re-export all its declarations.
This attribute is most commonly used by overlays.

```swift
// module M
func f() {}

// module N
@_exported import M

// module P
import N
func g() {
  N.f() // OK
}
```

## `@_fixed_layout`

## `@_hasInitialValue`

## `@_hasMissingDesignatedInitializers`

## `@_hasStorage`

## `@_implementationOnly`

Used to mark an imported module as an implementation detail.
This prevents types from that module being exposed in API
(types of public functions, constraints in public extension etc.)
and ABI (usage in `@inlinable` code).

## `@_implements`

## `@_implicitSelfCapture`

Allows access to `self` inside a closure without explicitly capturing it,
even when `Self` is a reference type.

```swift
class C {
  func f() {}
  func g(_: @escaping () -> Void {
    g({ f() }) // error: call to method 'f' in closure requires explicit use of 'self'
  }
  func h(@_implicitSelfCapture _: @escaping () -> Void) {
    h({ f() }) // ok
  }
}
```

## `@_inheritActorContext`

(Note that it is "inherit", not "inherits", unlike below.)

## `@_inheritsConvenienceInitializers`

## `@_marker`

Indicates that a protocol is a marker protocol;
one that represents some meaningful property at compile-time
but has no runtime representation.

For more details, see [SE-0302](https://github.com/apple/swift-evolution/blob/main/proposals/0302-concurrent-value-and-concurrent-closures.md#marker-protocols), which introduces marker protocols.

At the moment, the language only has 1 marker protocol: `Sendable`.

Fun fact: Rust has a very similar concept called
[marker traits](https://doc.rust-lang.org/std/marker/index.html),
including one called `Send`,
which inspired the design of `Sendable`.

## `@_nonEphemeral`

Marks a function parameter that cannot accept a temporary pointer produced from
an inout-to-pointer, array-to-pointer, or string-to-pointer conversion. Such a
parameter may only accept a pointer that is guaranteed to outlive the duration
of the function call.

Attempting to pass a temporary pointer to an `@_nonEphemeral` parameter will
produce a warning. This attribute is primarily used within the standard library
on the various `UnsafePointer` initializers to warn users about
the undefined behavior caused by using a temporary pointer conversion as an
argument:

```swift
func baz() {
  var x = 0

  // warning: Initialization of 'UnsafePointer<Int>' results in a dangling pointer
  let ptr = UnsafePointer(&x)

  // warning: Initialization of 'UnsafePointer<Int>' results in a dangling pointer
  let ptr2 = UnsafePointer([1, 2, 3])
}
```

The temporary pointer conversion produces a pointer that is only
guaranteed to be valid for the duration of the call to the initializer,
and becomes invalid once the call ends.
So the newly created `UnsafePointer` will be dangling.

One exception to this is that inout-to-pointer conversions
on static stored properties and global stored properties
produce non-ephemeral pointers, as long as they have no observers:

```swift
var global = 0

struct S {
  static var staticVar = 0
}

func baz() {
  let ptr = UnsafePointer(&global) // okay
  let ptr2 = UnsafePointer(&S.staticVar) // okay
}
```

Additionally, if they are of a tuple or struct type, their stored members
without observers may also be passed inout as non-ephemeral pointers.

For more details, see [the educational note on temporary pointer usage](/userdocs/diagnostics/temporary-pointers.md).

## `@_nonoverride`

## `@_objc_non_lazy_realization`

## `@_optimize(...)`

## `@_originallyDefinedIn`

Marks a declaration as being originally defined in a different module,
changing the name mangling. This can be used to move declarations
from a module to one of the modules it imports without breaking clients.

Consider the following example where a framework ToasterKit needs
to move some APIs to a lower-level framework ToasterKitCore.
Here are the necessary changes:

1. Add a linker flag `-reexport_framework ToasterKitCore` for ToasterKit.
   This ensures all symbols defined in ToasterKitCore will be accessible during
   runtime via ToasterKit, so existing apps continue to run.
2. In ToasterKit, use `@_exported import ToasterKitCore`.
   This ensures existing source code that only imports ToasterKit continues to
   type-check.
3. Move the necessary declarations from ToasterKit to ToasterKitCore.
   The moved declaration should have two attributes:
   - `@available` indicating when the declaration was introduced in ToasterKit.
   - `@_originallyDefinedIn` indicating the original module and when the
     declaration was moved to ToasterKitCore.
   ```
   @available(toasterOS 42, *)
   @_originallyDefinedIn(module: "ToasterKit", toasterOS 57)
   enum Toast {
   case underdone
   case perfect
   case burnt
   }
   ```
4. Add Swift compiler flags `-Xfrontend -emit-ldadd-cfile-path -Xfrontend /tmp/t.c`
   to ToasterKitCore's build settings. Add the emitted `/tmp/t.c` file to
   ToasterKit's compilation.
   This ensures when an app is built for deployment targets prior to the symbols' move,
   the app will look for these symbols in ToasterKit instead of ToasterKitCore.

## `@_private(sourceFile:)`

Fully bypasses access control, allowing access to private declarations
in the imported module. The imported module needs to be compiled with
`-Xfrontend -enable-private-imports` for this to work.

## `@_semantics`

Marks a function as having particular high-level semantics that are
specially recognized by the SIL optimizer.

## `@_show_in_interface`

## `@_silgen_name`

Changes the symbol name for a function, similar to an ASM label in C,
except that the platform symbol mangling (leading underscore on Darwin)
is maintained.

Since this has label-like behavior, it may not correspond to any declaration;
if so, it is assumed that the function is implemented in C.

A function defined by `@_silgen_name` is assumed to use the Swift ABI.

## `@_specialize(...)`

Forces generation of a specialized implementation for a generic declaration.

See [Generics.rst](/docs/Generics.rst) for more details.

## `@_specializeExtension`

## `@_spi(spiName)`

Marks a declaration as SPI (System Programming Interface), instead of API.
Modules exposing SPI and using library evolution generate an additional
`.private.swiftinterface` file (with `-emit-private-module-interface-path`)
in addition to the usual `.swiftinterface` file. This private interface exposes
both API and SPI.

Clients can access SPI by marking the import as `@_spi(spiName) import Module`.
This design makes it easy to find out which clients are using certain SPIs by
doing a textual search.

## `@_staticInitializeObjCMetadata`

## `@_transparent`

Marks a function to be "macro-like", i.e., it is guaranteed to be inlined
in debug builds.

See [TransparentAttr.md](/docs/TransparentAttr.md) for more details.

## `@_typeEraser`

## `@_weakLinked`

## `@_unsafeMainActor`

## `@_unsafeSendable`

Allows a function type to be treated as `@Sendable` without enforcement.

This is unsafe as it can allow access to shared mutable state without
synchronization, which would be an error with `@Sendable` enforcement.
