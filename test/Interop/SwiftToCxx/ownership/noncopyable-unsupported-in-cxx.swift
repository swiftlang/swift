// The cases a noncopyable Swift type is not yet exposed to C++ in, and the one
// resilience case it is.

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Unsup -enable-experimental-feature GenerateBindingsForNoncopyableTypesInCXX -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/unsup.h
// RUN: %FileCheck --implicit-check-not=NestedMO %s < %t/unsup.h
// RUN: %check-interop-cxx-header-in-clang(%t/unsup.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

// RUN: %target-swift-frontend %s -module-name Unsup -enable-library-evolution -enable-experimental-feature GenerateBindingsForNoncopyableTypesInCXX -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/unsup-evo.h
// RUN: %FileCheck --check-prefix=EVO %s < %t/unsup-evo.h
// RUN: %check-interop-cxx-header-in-clang(%t/unsup-evo.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

// REQUIRES: swift_feature_GenerateBindingsForNoncopyableTypesInCXX

public struct MO: ~Copyable {
    public let x: Int
    public init(x: Int) { self.x = x }
}

// A noncopyable payload would have to be moved into the case constructor.
public enum EnumWithMOPayload: ~Copyable {
    case a(MO)
    case b
}

// A generic noncopyable type is not exposed.
public struct Box<T: ~Copyable>: ~Copyable {
    public var t: T
    public init(t: consuming T) { self.t = t }
}

// A 'Mirror' field is resilient, which leaves the layout opaque.
public struct OpaqueMO: ~Copyable {
    public var o: Mirror
    public init(o: Mirror) { self.o = o }
}

public func makeOpaqueMO(_ o: Mirror) -> OpaqueMO { return OpaqueMO(o: o) }

// Using a noncopyable type as a Swift generic argument, 'Optional' included.
public func optMO() -> MO? { return nil }
public func takeOptMO(_ x: consuming MO?) {}
public func takePtr(_ p: UnsafeMutablePointer<MO>) {}

// A type nested in a generic one has no layout C++ can know either. It is
// dropped without a stub, as it is never visited as a top-level declaration.
public struct Generic<T> {
    public var t: T
    public init(t: T) { self.t = t }
    public struct NestedMO: ~Copyable {
        public var n: Int
        public init(n: Int) { self.n = n }
    }
}

// A zero sized value type is not exposed, with or without a 'deinit'.
public struct EmptyMO: ~Copyable {}

public struct EmptyDeinitMO: ~Copyable {
    deinit {}
}

@frozen
public struct FrozenMO: ~Copyable {
    public let x: Int
    public init(x: Int) { self.x = x }
}

// The rejections below must not be because 'MO' itself is not exposed.
// CHECK: class SWIFT_SYMBOL({{.*}}) MO final {

// CHECK-DAG: class Box { } SWIFT_UNAVAILABLE_MSG("noncopyable generic struct 'Box' can not yet be represented in C++");
// CHECK-DAG: class EnumWithMOPayload { } SWIFT_UNAVAILABLE_MSG("Swift enum 'EnumWithMOPayload' cannot be represented in C++");
// CHECK-DAG: class OpaqueMO { } SWIFT_UNAVAILABLE_MSG("noncopyable struct 'OpaqueMO' can not yet be represented in C++");
// CHECK-DAG: // Unavailable in C++: Swift global function 'makeOpaqueMO(_:)'. Return type 'OpaqueMO' is not representable in C++.
// CHECK-DAG: // Unavailable in C++: Swift global function 'optMO()'. Return type 'MO?' is not representable in C++.
// CHECK-DAG: // Unavailable in C++: Swift global function 'takeOptMO(_:)'. Parameter 'x' of type 'MO?' is not representable in C++.
// CHECK-DAG: // Unavailable in C++: Swift global function 'takePtr(_:)'. Parameter 'p' of type 'UnsafeMutablePointer<MO>' is not representable in C++.
// CHECK-DAG: class EmptyMO { } SWIFT_UNAVAILABLE_MSG("'EmptyMO' is a zero sized value type, it cannot be exposed to C++ yet");
// CHECK-DAG: class EmptyDeinitMO { } SWIFT_UNAVAILABLE_MSG("'EmptyDeinitMO' is a zero sized value type, it cannot be exposed to C++ yet");

// Under library evolution a non-frozen type has no layout C++ can know, while
// a frozen one is still exposed as a move-only class.
// EVO: class SWIFT_SYMBOL({{.*}}) FrozenMO final {
// EVO: SWIFT_INLINE_THUNK FrozenMO(FrozenMO &&other) noexcept {
// EVO: bool _isMovedFrom = false;
// EVO: class MO { } SWIFT_UNAVAILABLE_MSG("noncopyable struct 'MO' can not yet be represented in C++");
