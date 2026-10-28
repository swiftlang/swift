// RUN: %empty-directory(%t)
// RUN: split-file %s %t --leading-lines

/// Build a library exposing public @objc global functions.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %t/Lib.swift \
// RUN:   -emit-module -module-name Lib -o %t

/// A client in another module calls them. C-compatible functions call the C
/// symbol directly; bridged functions call the native Swift symbol.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %t/Client.swift \
// RUN:   -emit-ir -module-name Client -I %t | %FileCheck %t/Client.swift --check-prefix=CHECK-%target-ptrsize

// REQUIRES: objc_interop

//--- Lib.swift

import Foundation

@objc(renamed) public func named() -> Int { return 0 }

@objc(greetInLib) public func greet(name: String) -> String { return name }

//--- Client.swift

import Lib

// C-compatible: cross-module call uses the C symbol directly.
// CHECK-64-LABEL: define swiftcc i64 @"$s6Client5useItSiyF"
// CHECK-64: call i64 @renamed()
// CHECK-32-LABEL: define swiftcc i32 @"$s6Client5useItSiyF"
// CHECK-32: call i32 @renamed()
public func useIt() -> Int {
  return named()
}

// Bridged: cross-module call uses the native Swift symbol.
// CHECK-64-LABEL: define swiftcc { i64, ptr } @"$s6Client8useGreetSSyF"
// CHECK-64: call swiftcc { i64, ptr } @"$s3Lib5greet4nameS2S_tF"
// CHECK-32-LABEL: define swiftcc { i32, i32, i32 } @"$s6Client8useGreetSSyF"
// CHECK-32: call swiftcc { i32, i32, i32 } @"$s3Lib5greet4nameS2S_tF"
public func useGreet() -> String {
  return greet(name: "World")
}
