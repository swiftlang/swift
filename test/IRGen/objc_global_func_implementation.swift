// RUN: %empty-directory(%t)
// RUN: split-file %s %t --leading-lines

/// Build a module containing an @objc @implementation definition and verify
/// that its native body remains an implementation detail. Calls in both the
/// defining module and a dependent module must use the imported C entry point.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %t/Lib.swift \
// RUN:   -import-objc-header %t/ObjCAPI.h -emit-module -module-name Lib \
// RUN:   -emit-module-path %t/Lib.swiftmodule \
// RUN:   -emit-module-interface-path %t/Lib.swiftinterface
// RUN: %FileCheck %s --check-prefix=INTERFACE < %t/Lib.swiftinterface
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %t/Lib.swift \
// RUN:   -import-objc-header %t/ObjCAPI.h -emit-ir -module-name Lib \
// RUN:   | %FileCheck %t/Lib.swift --check-prefix=LIB
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %t/Client.swift \
// RUN:   -import-objc-header %t/ObjCAPI.h -emit-ir -module-name Client -I %t \
// RUN:   | %FileCheck %t/Client.swift --check-prefix=CLIENT

// REQUIRES: objc_interop

// INTERFACE-NOT: ObjCImplGreet
// INTERFACE: public func sameModuleCall
// INTERFACE-NOT: ObjCImplGreet

//--- ObjCAPI.h

@class NSString;

extern NSString * _Nonnull ObjCImplGreet(NSString * _Nonnull name);

//--- Lib.swift

import Foundation

@objc @implementation
// The C entry point is the only client-facing ABI. The compiler may still
// emit a native Swift body as an implementation detail behind its bridge.
// LIB-DAG: define ptr @ObjCImplGreet(ptr
// LIB-DAG: define swiftcc {{.*}} @"$s3Lib13ObjCImplGreetyS2SF"
public func ObjCImplGreet(_ name: String) -> String {
  return "Hello, \(name)"
}

// LIB-LABEL: define swiftcc {{.*}} @"$s3Lib14sameModuleCallyS2SF"
// LIB-NOT: call swiftcc {{.*}} @"$s3Lib13ObjCImplGreetyS2SF"
// LIB: call ptr @ObjCImplGreet(ptr
// LIB-NOT: call swiftcc {{.*}} @"$s3Lib13ObjCImplGreetyS2SF"
public func sameModuleCall(_ name: String) -> String {
  return ObjCImplGreet(name)
}

//--- Client.swift

import Lib

// CLIENT-LABEL: define swiftcc {{.*}} @"$s6Client10clientCallyS2SF"
// CLIENT-NOT: call swiftcc {{.*}} @"$s3Lib13ObjCImplGreetyS2SF"
// CLIENT: call ptr @ObjCImplGreet(ptr
// CLIENT-NOT: call swiftcc {{.*}} @"$s3Lib13ObjCImplGreetyS2SF"
public func clientCall(_ name: String) -> String {
  return ObjCImplGreet(name)
}
