// RUN: %empty-directory(%t)
// RUN: split-file %s %t --leading-lines

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) \
// RUN:   -emit-module %t/Lib.swift -o %t -I %t \
// RUN:   -swift-version 6 -enable-library-evolution \
// RUN:   -enable-experimental-feature ObjCAttrAllowsCArrays \
// RUN:   -emit-module-interface-path %t/Lib.swiftinterface

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %t \
// RUN:   -typecheck-module-from-interface %t/Lib.swiftinterface

// RUN: %FileCheck %s --input-file %t/Lib.swiftinterface

// REQUIRES: objc_interop
// REQUIRES: swift_feature_ObjCAttrAllowsCArrays

//--- module.modulemap
module Lib {
    header "Lib.h"
}

//--- Lib.h
extern void cImplFunc();
extern void objcImplFunc();

//--- Lib.swift
@_exported import Lib
import Foundation

/// Don't print the implementation functions.
// CHECK-NOT: @implementation
// CHECK-NOT: ImplFunc

@c @implementation
public func cImplFunc() { }

@_cdecl("objcImplFunc") @implementation
public func objcImplFunc() { }

/// Print other @c functions.
@c
public func bareCDecl() {}
// CHECK: #if compiler(>=5.3) && hasAttribute(c)
// CHECK-NEXT: @c
// CHECK-NEXT: public func bareCDecl
// CHECK-NEXT: #else
// CHECK-NEXT: @_cdecl("bareCDecl")
// CHECK-NEXT: public func bareCDecl
// CHECK-NEXT: #endif
@c(c_name)
public func namedCDecl() {}
// CHECK: @c(c_name)
// CHECK-NEXT: public func namedCDecl

@_cdecl("objc_name")
public func namedLegacyCDecl() {}
// CHECK: @_cdecl("objc_name")
// CHECK-NEXT: public func namedLegacyCDecl

/// Pointers to C arrays in @c declarations need the feature that allows them.
/// Without it, the declaration is left out because dropping @c would change
/// the function's symbol.
@c
public func cArrayPointer(_ p: UnsafeMutablePointer<(Int32, Int32)>) {}
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: #if compiler(>=5.3) && hasAttribute(c)
// CHECK-NEXT: @c
// CHECK-NEXT: public func cArrayPointer
// CHECK-NEXT: #else
// CHECK-NEXT: @_cdecl("cArrayPointer")
// CHECK-NEXT: public func cArrayPointer
// CHECK-NEXT: #endif
// CHECK-NEXT: #endif
