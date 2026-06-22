// RUN: %empty-directory(%t)
// RUN: split-file %s %t --leading-lines

/// Build a library exposing a public @objc global function.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %t/Lib.swift \
// RUN:   -emit-module -module-name Lib -o %t

/// A client in another module calls it by its Swift base name. The call uses
/// the native Swift symbol (swiftcc); the C symbol is the library's thunk.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %t/Client.swift \
// RUN:   -emit-ir -module-name Client -I %t | %FileCheck %t/Client.swift

// REQUIRES: objc_interop

//--- Lib.swift

import Foundation

@objc(renamed) public func named() -> Int { return 0 }

//--- Client.swift

import Lib

// The client calls the public function by its Swift base name. Cross-module
// calls use the native Swift-convention symbol.
// CHECK-LABEL: define swiftcc i64 @"$s6Client5useItSiyF"
// CHECK: call swiftcc i64 @"$s3Lib5namedSiyF"()
public func useIt() -> Int {
  return named()
}
