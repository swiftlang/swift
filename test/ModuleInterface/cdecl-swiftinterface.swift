// RUN: %empty-directory(%t)
// RUN: split-file %s %t --leading-lines

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) \
// RUN:   -emit-module %t/Lib.swift -o %t -I %t \
// RUN:   -swift-version 6 -enable-library-evolution \
// RUN:   -emit-module-interface-path %t/Lib.swiftinterface \
// RUN:   -enable-experimental-feature CDecl \
// RUN:   -enable-experimental-feature CImplementation

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %t \
// RUN:   -typecheck-module-from-interface %t/Lib.swiftinterface

// RUN: %FileCheck %s --input-file %t/Lib.swiftinterface

// REQUIRES: objc_interop
// REQUIRES: swift_feature_CDecl
// REQUIRES: swift_feature_CImplementation

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
// CHECK: @c
// CHECK-NEXT: public func bareCDecl

@c(c_name)
public func namedCDecl() {}
// CHECK: @c(c_name)
// CHECK-NEXT: public func namedCDecl

@_cdecl("objc_name")
public func namedLegacyCDecl() {}
// CHECK: @_cdecl("objc_name")
// CHECK-NEXT: public func namedLegacyCDecl
