// REQUIRES: objc_interop

// RUN: %empty-directory(%t)

// FIXME: BEGIN -enable-source-import hackaround
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t %S/../Inputs/clang-importer-sdk/swift-modules/ObjectiveC.swift -disable-objc-attr-requires-foundation-module -target %target-swift-5.1-abi-triple
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/CoreGraphics.swift -target %target-swift-5.1-abi-triple
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/Foundation.swift -target %target-swift-5.1-abi-triple
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/AppKit.swift -target %target-swift-5.1-abi-triple
// FIXME: END -enable-source-import hackaround

// REQUIRES: concurrency

// RUN: %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -parse-as-library %s -typecheck -verify -emit-objc-header-path %t/async.h -I %S/Inputs/custom-modules -import-objc-header %S/../Inputs/empty.h -target %target-swift-5.1-abi-triple
// RUN: %FileCheck %s < %t/async.h
// RUN: %check-in-clang -I %S/Inputs/custom-modules/ %t/async.h

import Foundation

// CHECK-LABEL: @interface BarClass : NSObject
@objc @objcMembers class BarClass: NSObject {
  // CHECK: (void)doSomethingBigWithCompletionHandler:(void (^ _Nonnull)(NSInteger))completionHandler;
  func doSomethingBig() async -> Int { 0 }

  // CHECK: - (void)longRunningWithString:(NSString * _Nonnull)string completionHandler:(void (^ _Nonnull)(BarClass * _Nullable, NSError * _Nullable))completionHandler;
  func longRunning(string: String) async throws -> BarClass { return self }

  // CHECK: - (void)magicTupleReturnWithCompletionHandler:(void (^ _Nonnull)(BarClass * _Nonnull, NSInteger))completionHandler;
  func magicTupleReturn() async -> (BarClass, Int) { return (self, 0) }
}
// CHECK: @end
