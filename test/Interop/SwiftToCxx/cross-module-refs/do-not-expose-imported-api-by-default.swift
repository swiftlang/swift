// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %S/Inputs/structs.swift -module-name Structs -emit-module -emit-module-path %t/Structs.swiftmodule

// RUN: %target-swift-frontend %s -module-name UsesStructs -I %t -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/uses-structs.h

// RUN: %check-interop-cxx-header-in-clang(%t/uses-structs.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)
// RUN: %FileCheck %s < %t/uses-structs.h

import Structs

public struct StructExposed {
    public func availableInHeader() -> Int {
        return 0
    }

    public func unavailableInHeader(_ y: StructSeveralI64) -> StructSeveralI64 {
        return y
    }

    public let unavailableInHeaderProp: StructSeveralI64
}

public func unavailableInHeaderFunc(_ x: StructSeveralI64) -> StructSeveralI64 {
    return Structs.passThroughStructSeveralI64(i: 0, x, j: 2)
}

// CHECK: // Unavailable in C++: Swift global function 'unavailableInHeaderFunc(_:)'.
