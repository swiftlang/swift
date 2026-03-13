// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %S/resilient-struct-in-cxx.swift -enable-library-evolution -module-name Structs -emit-module -emit-module-path %t/Structs.swiftmodule

// RUN: %target-swift-frontend %s -module-name UseStructs -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/useStructs.h -I %t
// RUN: %FileCheck %s < %t/useStructs.h

// RUN: %check-interop-cxx-header-in-clang(%t/useStructs.h)

import Structs

public struct UsesResilientSmallStruct {
    let x: UInt32
    let y: FirstSmallStruct

    public func dump() {
        print("UsesResilientSmallStruct(\(x),\(y)")
    }
}

// CHECK: class SWIFT_SYMBOL("s:10UseStructs24UsesResilientSmallStructV") UsesResilientSmallStruct final {
// CHECK:   SWIFT_INLINE_THUNK const char * _Nonnull _getOpaquePointer() const noexcept { return _storage.getOpaquePointer(); }
// CHECK: swift::_impl::OpaqueStorage _storage;

public func createUsesResilientSmallStruct() -> UsesResilientSmallStruct {
    UsesResilientSmallStruct(x: 97, y: createLargeStruct(45).firstSmallStruct)
}
