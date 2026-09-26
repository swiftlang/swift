// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %S/Inputs/structs.swift -module-name Structs -emit-module -emit-module-path %t/Structs.swiftmodule

// RUN: %target-swift-frontend %s -module-name UsesStructs -I %t -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/uses-structs.h

// RUN: %check-interop-cxx-header-in-clang(%t/uses-structs.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)
// RUN: %FileCheck %s < %t/uses-structs.h

import Structs

public final class ClassExposed {
    public init() {}

    public func availableInHeader() -> Int {
        return 0
    }

    public func unavailableInHeader(_ y: StructSeveralI64) -> StructSeveralI64 {
        return y
    }
}

// CHECK: class SWIFT_SYMBOL("s:11UsesStructs12ClassExposedC") ClassExposed final : public swift::_impl::RefCountedClass {
// CHECK:        SWIFT_INLINE_THUNK swift::Int availableInHeader() noexcept SWIFT_SYMBOL("s:11UsesStructs12ClassExposedC17availableInHeaderSiyF");
// CHECK-NEXT:   // Unavailable in C++: Swift instance method 'unavailableInHeader(_:)'. Return type 'StructSeveralI64' is not representable in C++ because module 'Structs' is not exposed.
// CHECK-NEXT: protected:

public struct StructExposed {
    public func availableInHeader() -> Int {
        return 0
    }

    public func unavailableInHeader(_ y: StructSeveralI64) -> StructSeveralI64 {
        return y
    }

    public let unavailableInHeaderProp: StructSeveralI64
}

// CHECK: class SWIFT_SYMBOL("s:11UsesStructs13StructExposedV") StructExposed final {
// CHECK:        SWIFT_INLINE_THUNK swift::Int availableInHeader() const noexcept SWIFT_SYMBOL("s:11UsesStructs13StructExposedV17availableInHeaderSiyF");
// CHECK-NEXT:   // Unavailable in C++: Swift instance method 'unavailableInHeader(_:)'. Return type 'StructSeveralI64' is not representable in C++ because module 'Structs' is not exposed.
// CHECK-NEXT:   // Unavailable in C++: Swift property 'unavailableInHeaderProp'. Return type 'StructSeveralI64' is not representable in C++ because module 'Structs' is not exposed.
// CHECK-NEXT: private:

public func unavailableInHeaderFunc(_ x: StructSeveralI64) -> StructSeveralI64 {
    return Structs.passThroughStructSeveralI64(i: 0, x, j: 2)
}

public func unavailableInHeaderGenericArg(_ x: [StructSeveralI64]) {}

// Closures can't be represented in C++ at all, so the reason doesn't name the
// module.
public func unavailableInHeaderClosure(_ x: (StructSeveralI64) -> Void) {}

// CHECK: // Unavailable in C++: Swift global function 'unavailableInHeaderClosure(_:)'. Parameter 'x' of type '(StructSeveralI64) -> Void' is not representable in C++.{{$}}
// CHECK: // Unavailable in C++: Swift global function 'unavailableInHeaderFunc(_:)'. Return type 'StructSeveralI64' is not representable in C++ because module 'Structs' is not exposed.
// CHECK: // Unavailable in C++: Swift global function 'unavailableInHeaderGenericArg(_:)'. Parameter 'x' of type '[StructSeveralI64]' is not representable in C++ because module 'Structs' is not exposed.
