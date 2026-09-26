// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Generics -clang-header-expose-decls=has-expose-attr -enable-experimental-feature GenerateBindingsForHashableRequirementsInCXX -typecheck -verify

// RUN: grep -v _expose %s > %t/clean.swift
// RUN: %target-swift-frontend %t/clean.swift -module-name Generics -clang-header-expose-decls=all-public -enable-experimental-feature GenerateBindingsForHashableRequirementsInCXX -typecheck -verify -emit-clang-header-path %t/generics.h
// RUN: %FileCheck %s --implicit-check-not=onlyWhenUIsComparable --implicit-check-not=FourArgumentInner --implicit-check-not=requiresComparable --implicit-check-not='bool operator []' --implicit-check-not=lE9hashValue < %t/generics.h

// RUN: %check-interop-cxx-header-in-clang(%t/generics.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

// REQUIRES: swift_feature_GenerateBindingsForHashableRequirementsInCXX

public func genericRequirementHashable<T: Hashable>(_ x: T) {}

// Generic requirement parameters are unnamed because multiple generic
// parameters can each require the same protocol.
public func genericRequirementTwoHashableParameters<T: Hashable, U: Hashable>(
    _ x: T, _ y: U
) {}

// Swift generic requirements are erased from C++ function signatures. When a
// constrained and an unconstrained overload collide, keep the unconstrained
// one in either source order, so that calls with non-Hashable types remain
// valid.
public func genericOverloadUnconstrainedFirst<T>(_ x: T) {}
public func genericOverloadUnconstrainedFirst<T: Hashable>(_ x: T) {}

public func genericOverloadConstrainedFirst<T: Hashable>(_ x: T) {}
public func genericOverloadConstrainedFirst<T>(_ x: T) {}

// A marker protocol requirement needs no witness table, so an overload that
// only has one is preferred over a Hashable-constrained overload.
public func genericOverloadHashableBeforeMarker<T: Hashable>(_ x: T) {}
public func genericOverloadHashableBeforeMarker<T: Sendable>(_ x: T) {}

public struct HashableMethods {
    var x: Int

    public init() { x = 0 }

    public func genericMethodHashable<T: Hashable>(_ x: T) -> Bool {
        return x == x
    }
}

// Only direct Hashable requirements are supported. This does not expose other
// protocols, protocol existential values, or opaque result types to C++.
public func requiresEquatable<Value: Equatable>(_ value: Value) {}

public func acceptsAnyHashable(_ value: any Hashable) {}

public func returnsAnyHashable() -> any Hashable {
    return 0
}

public func returnsSomeHashable() -> some Hashable {
    return 0
}

// A Hashable requirement on a parameter pack would need a witness table pack.
@available(SwiftStdlib 5.9, *)
public struct HashablePack<each T: Hashable> {
    var v: Int
}

public struct Box<T: Hashable, U> {
    var t: T
    var u: U

    public init(t: T, u: U) {
        self.t = t
        self.u = u
    }

    public func alwaysAvailable() -> Int { return 1 }

    public subscript(index: Int) -> Int { return 1 }
}

// A supported requirement added by a conditional extension is checked by the
// same runtime witness lookup as a requirement declared directly on a member.
extension Box where U: Hashable {
    public func onlyWhenUIsHashable() -> Int { return 2 }

    // Erases to the same C++ parameter types as the unconstrained subscript
    // above, so only the unconstrained one is exposed.
    public subscript(index: Int) -> Bool { return true }
}

// A constrained member of the type collides with an unconstrained member of an
// extension. The unconstrained one is kept.
public struct ConstrainedMemberFirst<T> {
    var t: T

    public init(_ t: T) {
        self.t = t
    }

    public func describe() -> Int where T: Hashable { return 1 }
}

extension ConstrainedMemberFirst {
    public func describe() -> Int { return 0 }
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
@_expose(Cxx) // expected-error {{generic struct 'TooManyGenericRequirements' can not yet be represented in C++ as it has more than 3 generic parameters and 'Hashable' requirements combined}}
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

// Standard library members that need an extra Hashable conformance, like
// `hashValue` from `extension Array: Hashable where Element: Hashable`, are
// not exposed (see the implicit-check-not above).

// A Hashable requirement is passed as a witness table, after the type metadata.
// CHECK: SWIFT_EXTERN void $s8Generics26genericRequirementHashableyyxSHRzlF(const void * _Nonnull x, void * _Nonnull , void * _Nonnull ) SWIFT_NOEXCEPT SWIFT_CALL; // genericRequirementHashable(_:)
// CHECK-NEXT: SWIFT_EXTERN void $s8Generics39genericRequirementTwoHashableParametersyyx_q_tSHRzSHR_r0_lF(const void * _Nonnull x, const void * _Nonnull y, void * _Nonnull , void * _Nonnull , void * _Nonnull , void * _Nonnull ) SWIFT_NOEXCEPT SWIFT_CALL; // genericRequirementTwoHashableParameters(_:_:)

// CHECK-LABEL: class SWIFT_SYMBOL("s:8Generics3BoxV") Box final {
// CHECK: SWIFT_INLINE_THUNK swift::Int alwaysAvailable() const noexcept
// CHECK-NEXT: SWIFT_INLINE_THUNK swift::Int operator [](swift::Int index) const noexcept SWIFT_SYMBOL("s:8Generics3BoxVyS2icig");
// CHECK-NEXT: SWIFT_INLINE_THUNK swift::Int onlyWhenUIsHashable() const noexcept
// CHECK-NEXT: // skip emitting subscript 'subscript(_:) -> Bool'. 'operator []' with the same parameter types already declared.
// CHECK-NEXT: SWIFT_INLINE_THUNK swift::Int implicitlySatisfiedRequirement() const noexcept

// CHECK-LABEL: class SWIFT_SYMBOL("s:8Generics22ConstrainedMemberFirstV") ConstrainedMemberFirst final {
// CHECK: SWIFT_INLINE_THUNK swift::Int describe() const noexcept SWIFT_SYMBOL("s:8Generics22ConstrainedMemberFirstV8describeSiyF");
// CHECK-NOT: describe()

// CHECK-LABEL: class SWIFT_SYMBOL("s:8Generics21ConstrainedSubscriptsV") ConstrainedSubscripts final {
// CHECK: SWIFT_INLINE_THUNK swift::Int operator [](const T_0_0& value) const noexcept SWIFT_SYMBOL("s:8Generics21ConstrainedSubscriptsVySixcSHRzluig");
// CHECK-NOT: operator []

// CHECK-LABEL: class SWIFT_SYMBOL("s:8Generics30ContextuallyConstrainedMembersV") ContextuallyConstrainedMembers final {
// CHECK: SWIFT_INLINE_THUNK void requiresHashable() const noexcept SWIFT_SYMBOL("s:8Generics30ContextuallyConstrainedMembersV16requiresHashableyySHRzlF");

// CHECK-LABEL: class SWIFT_SYMBOL("s:8Generics15HashableMethodsV") HashableMethods final {
// CHECK: SWIFT_INLINE_THUNK bool genericMethodHashable(const T_0_0& x) const noexcept SWIFT_SYMBOL("s:8Generics15HashableMethodsV013genericMethodB0ySbxSHRzlF");

// Only the unconstrained overloads are printed.
// CHECK: SWIFT_INLINE_THUNK void genericOverloadConstrainedFirst(const T_0_0& x) noexcept SWIFT_SYMBOL("s:8Generics31genericOverloadConstrainedFirstyyxlF") {
// CHECK: SWIFT_INLINE_THUNK void genericOverloadHashableBeforeMarker(const T_0_0& x) noexcept SWIFT_SYMBOL("s:8Generics35genericOverloadHashableBeforeMarkeryyxs8SendableRzlF") {
// CHECK: SWIFT_INLINE_THUNK void genericOverloadUnconstrainedFirst(const T_0_0& x) noexcept SWIFT_SYMBOL("s:8Generics33genericOverloadUnconstrainedFirstyyxlF") {

// The witness table for a Hashable requirement is looked up at runtime.
// CHECK: SWIFT_INLINE_THUNK void genericRequirementHashable(const T_0_0& x) noexcept SWIFT_SYMBOL("s:8Generics26genericRequirementHashableyyxSHRzlF") {
// CHECK: Generics::_impl::$s8Generics26genericRequirementHashableyyxSHRzlF(swift::_impl::getOpaquePointer(x), swift::TypeMetadataTrait<T_0_0>::getTypeMetadata(), swift::_impl::getConformanceWitnessTable<T_0_0, swift::_impl::HashableProtocolDescriptor>());
// CHECK: SWIFT_INLINE_THUNK void genericRequirementTwoHashableParameters(const T_0_0& x, const T_0_1& y) noexcept
// CHECK: swift::_impl::getConformanceWitnessTable<T_0_0, swift::_impl::HashableProtocolDescriptor>(), swift::_impl::getConformanceWitnessTable<T_0_1, swift::_impl::HashableProtocolDescriptor>());

// A requirement added by an extension is looked up for the right parameter.
// CHECK: SWIFT_INLINE_THUNK swift::Int Box<T_0_0, T_0_1>::onlyWhenUIsHashable() const noexcept {
// CHECK: swift::_impl::getConformanceWitnessTable<T_0_1, swift::_impl::HashableProtocolDescriptor>(), _getOpaquePointer());

// CHECK: SWIFT_INLINE_THUNK bool HashableMethods::genericMethodHashable(const T_0_0& x) const noexcept {
// CHECK: swift::_impl::getConformanceWitnessTable<T_0_0, swift::_impl::HashableProtocolDescriptor>());

// CHECK: class HashablePack { } SWIFT_UNAVAILABLE_MSG("generic requirements for generic struct 'HashablePack' can not yet be represented in C++");

// CHECK-LABEL: class TooManyGenericRequirements { } SWIFT_UNAVAILABLE_MSG("generic struct 'TooManyGenericRequirements' can not yet be represented in C++ as it has more than 3 generic parameters and 'Hashable' requirements combined");
// CHECK: // Unavailable in C++: Swift global function 'acceptsAnyHashable(_:)'. Parameter 'value' of type 'any Hashable' is not representable in C++.
// CHECK: // Unavailable in C++: Swift global function 'genericOverloadConstrainedFirst(_:)'. An overload with the same C++ parameter types already exists.
// CHECK: // Unavailable in C++: Swift global function 'genericOverloadHashableBeforeMarker(_:)'. An overload with the same C++ parameter types already exists.
// CHECK: // Unavailable in C++: Swift global function 'genericOverloadUnconstrainedFirst(_:)'. An overload with the same C++ parameter types already exists.
// CHECK: // Unavailable in C++: Swift global function 'requiresEquatable(_:)'. generic requirements for global function 'requiresEquatable' can not yet be represented in C++.
// CHECK: // Unavailable in C++: Swift global function 'returnsAnyHashable()'. Return type 'any Hashable' is not representable in C++.
// CHECK: // Unavailable in C++: Swift global function 'returnsSomeHashable()'. Return type 'some Hashable' is not representable in C++.
