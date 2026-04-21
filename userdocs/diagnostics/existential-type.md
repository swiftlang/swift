# Existential Types and Performance (ExistentialType)

## Overview

Existential types enable runtime polymorphism by allowing a variable to store a
type-erased value - a value whose concrete type is not known at compile time,
but is known to conform to a specified protocol.

## Performance Costs

To accommodate the variation in size and layout of the concrete values at
runtime, existentials are implemented as containers with two main components:

1. A pointer to the heap-allocated value (values up to 24 bytes are inlined).
2. A witness table to extract the concrete type and dispatch to the
   corresponding concrete implementation.

This runtime abstraction imposes performance penalties rendering existentials
undesirable for performance-critical code paths.

1. ***Exclusivity Enforcement*** - Existential containers obscure the concrete
   type from the compiler, preventing static verification of Swift's [Law of
   Exclusivity](https://github.com/swiftlang/swift-evolution/blob/main/proposals/0176-enforce-exclusive-access-to-memory.md).
   This forces the compiler to insert dynamic runtime checks on each access to
   prevent simultaneous conflicting accesses. These checks verify at runtime
   that the existential value is not being accessed elsewhere, potentially
   causing a runtime trap if exclusivity is violated. In tight loops or
   performance-critical paths with frequent property or method access on
   existentials, these dynamic exclusivity checks dominate the combined cost of
   heap allocation and dynamic dispatch. The type-erased nature of existentials
   makes it particularly difficult for the compiler to optimize away these
   checks through static analysis.

2. ***Reference Counting*** - Existentials use the witness table to implement
   retain/release operations on reference-counted values (both reference and
   value types) during copy, move, and destroy operations. In addition to the
   overhead of calling these functions, this indirection prevents inlining of
   the reference counting logic.

3. ***Heap Allocation*** - Existentials use heap allocation for all values
   larger than 24 bytes, including value types like structs. Heap allocations
   are costly, and accessing a heap-allocated value via a pointer incurs
   indirection overhead with potential for cache misses.

4. ***Dynamic Dispatch*** - Method calls on existential types always use dynamic
   dispatch via the witness table, even for non-virtual methods, since the
   concrete type of the value is not known at compile time. Apart from the
   overhead of calling these methods indirectly via the witness table, the
   indirection prevents standard compiler optimizations such as function
   inlining and specialization.

## Alternatives

Given the significant runtime costs imposed by the use of existentials, it is
only recommended to use existentials sparingly in performance-critical code,
preferably at API boundaries or for heterogeneous collections where performance
is not critical. Various solutions to eliminate the use of existentials require
redesigning the code to avoid runtime polymorphism.

### 1. Concrete Types

If possible, change the implementation to use concrete types. Function
overloading can be used to implement separate functions with the same name for
every concrete type. Knowing the exact object type at compile time enables the
whole suite of compiler optimizations.

```swift
protocol Editor { ... }
struct Emacs: Editor { ... }
struct Vim: Editor { ... }

func moveCursor(_ emacs: Emacs) { ... }
func moveCursor(_ vim: Vim) { ... }
```

### 2. Generic Constraints

For functions that follow the same template for all concrete types conforming to
a protocol, [generic
constraints](https://docs.swift.org/swift-book/documentation/the-swift-programming-language/generics/#Type-Constraints)
can be used instead of repeating the code manually with function overloading.
The compiler will instantiate the code for all concrete types, ensuring static
dispatch, inlining, stack allocation for value types, and other standard
compiler optimizations blocked by the indirection from dynamic dispatch. This is
especially useful for homogeneous collections.

```swift
func processEditors<T: Editor>(_ editors: [T]) {
    for editor in editors {
        editor.moveCursor()
    }
}

let editors: [Emacs] = [Emacs(), Emacs(), Emacs()]
processEditors(editors)
```

### 3. Opaque Types

If a single concrete type that conforms to a protocol is being used on all
paths, but the concrete type needs to stay hidden, consider using an [opaque
type](https://docs.swift.org/swift-book/documentation/the-swift-programming-language/opaquetypes/).
Opaque types afford the flexibility of switching to a different concrete type as
long as it conforms to the same protocol. Since the compiler knows the concrete
type, the code will not incur any existential container overhead and method
calls will be dispatched statically unless the opaque type is used across module
boundaries and cross-module optimization is disabled or the function cannot be
inlined.

```swift
func instantiateBestEditor() -> some Editor {
    return Emacs()  // Compiler knows return type is Emacs
}

func getDeprecatedEditorPair() -> (some Editor, some Editor) {
    let vim1 = Vim()
    let vim2 = Vim()
    return (vim1, vim2)
}
```

### 4. Enum Types

Use an
[enum](https://docs.swift.org/swift-book/documentation/the-swift-programming-language/enumerations/)
to model the domain if there is a known fixed set of types conforming to a
protocol that cannot be extended by clients. Dispatch on `enum` cases incurs no
overhead since it is optimized to a jump table. There is also no performance
loss from pointer indirection since enums are stack allocated.

```swift
enum Editor {
    case emacs(Emacs)
    case vim(Vim)

    func moveCursor() {
        switch self {
        case .emacs(let e): ...
        case .vim(let v): ...
        }
    }
}
```

### 5. Type Erasure

Type erasure is a technique that hides the concrete types of constrained objects
behind a single type. Conceptually, this technique involves explicitly
implementing a witness-table-like functionality in Swift, mimicking existential
containers. This is particulary useful for heterogeneous collections. In the
following example, we intend to call `moveCursor` on a heterogeneous collection
of `Editor` types. So we wrap all `Editor` types inside an `AnyEditor` struct
that implements a generic initializer to capture the concrete type and invoke
its `moveCursor` implementation inside a closure. This approach can be easily
extended to other methods. While the code still incurs the indirection cost of
invoking the methods via closures, it can be controlled and limited to specific
methods.

```swift
struct AnyEditor: Editor {
    private let _moveCursor: () -> Void

    init<T: Editor>(_ editor: T) {
        _moveCursor = { editor.moveCursor() }
    }

    func moveCursor() {
        _moveCursor()
    }
}

let editors: [AnyEditor] = [
    AnyEditor(Emacs()),
    AnyEditor(Vim())
]
```

## Summary

Existential types trade performance for flexibility - values are heap allocated
and accessed indirectly through a pointer, reference counting is not inlined,
and methods are dispatched dynamically. It is strongly recommended to refactor
hot paths in the code to eliminate runtime polymorphism. Use concrete types,
generic constraints, or opaque types to ensure that the types are known at
compile time. Use enum if the set of cases is fixed and known ahead-of-time. Use
type erasure for heterogeneous collections.
