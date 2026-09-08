// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Generics -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/generics.h
// RUN: %FileCheck %s --implicit-check-not=onlyWhenUIsComparable --implicit-check-not=FourArgumentInner --implicit-check-not=requiresComparable --implicit-check-not='protocol descriptor for Comparable' < %t/generics.h

// RUN: %check-interop-cxx-header-in-clang(%t/generics.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

public struct Box<T: Hashable, U> {
    var t: T
    var u: U

    public init(t: T, u: U) {
        self.t = t
        self.u = u
    }

    public func alwaysAvailable() -> Int { return 1 }
}

// A supported requirement added by a conditional extension is checked by the
// same runtime witness lookup as a requirement declared directly on a member.
extension Box where U: Hashable {
    public func onlyWhenUIsHashable() -> Int { return 2 }
}

// Unsupported requirements still make the extension's members unavailable.
extension Box where U: Comparable {
    public func onlyWhenUIsComparable() -> Int { return 4 }
}

// An extension whose requirements are already implied by the type's own
// generic signature is exposed as usual.
extension Box where T: Hashable {
    public func implicitlySatisfiedRequirement() -> Int { return 3 }
}

// The generated C++ binding implements the direct form of a generic type
// metadata accessor, which accepts at most three arguments. Larger accessors
// use an indirect argument buffer that the binding does not implement yet.
// Witness tables count toward the direct limit, so this type cannot be
// represented: its accessor would need metadata for A, B, and C, plus the
// A: Hashable witness table.
public struct TooManyGenericRequirements<A: Hashable, B, C> {
    var a: A
    var b: B
    var c: C
}

// A nested nominal can inherit generic requirements from its context without
// having a generic parameter list of its own. Count those inherited metadata
// accessor arguments too.
public struct ContextuallyGenericOuter<A, B, C> {
    var a: A
    var b: B
    var c: C

    public init(a: A, b: B, c: C) {
        self.a = a
        self.b = b
        self.c = c
    }

    public struct FourArgumentInner where A: Hashable {
        var a: A
        var b: B
        var c: C

        public init(a: A, b: B, c: C) {
            self.a = a
            self.b = b
            self.c = c
        }
    }
}

// A member can add a requirement to a parameter from its enclosing type
// without having a generic parameter list of its own. Such contextually
// generic declarations must still go through requirement validation.
public struct ContextuallyConstrainedMembers<Value> {
    private var value: Value

    public init(_ value: Value) {
        self.value = value
    }

    public func requiresHashable() where Value: Hashable {}
    public func requiresComparable() where Value: Comparable {}
}

// Subscripts are generic contexts in their own right rather than abstract
// function declarations. Their requirements must pass through the same
// validation before their accessor thunks are printed.
public struct ConstrainedSubscripts {
    private var marker: Int

    public init() {
        marker = 0
    }

    public subscript<Value: Hashable>(_ value: Value) -> Int {
        return 1
    }

    public subscript<Value: Comparable>(comparable value: Value) -> Int {
        return 2
    }
}

// CHECK-LABEL: class SWIFT_SYMBOL("s:8Generics3BoxV") Box final {
// CHECK-DAG: alwaysAvailable()
// CHECK-DAG: implicitlySatisfiedRequirement()
// CHECK-DAG: onlyWhenUIsHashable()
// CHECK-LABEL: class SWIFT_SYMBOL("s:8Generics21ConstrainedSubscriptsV") ConstrainedSubscripts final {
// CHECK: operator [](
// CHECK-LABEL: class SWIFT_SYMBOL("s:8Generics30ContextuallyConstrainedMembersV") ContextuallyConstrainedMembers final {
// CHECK: requiresHashable()
// CHECK-LABEL: class TooManyGenericRequirements { } SWIFT_UNAVAILABLE_MSG(
