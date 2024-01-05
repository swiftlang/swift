// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %s -parse-as-library -emit-objc-header-path %t/swift.h
// RUN: %FileCheck %s < %t/swift.h

// REQUIRES: concurrency
// REQUIRES: asserts
// REQUIRES: objc_interop

import Foundation

@objc public protocol P {}

@objc public class Klass : NSObject {
  // CHECK: - (void)test1:(NSDictionary<NSString *, id> * _Nonnull)_;
  @objc public func test1(_: [String: any Sendable]) {}
  // CHECK: - (void)test2:(NSDictionary<NSString *, id <P>> * _Nonnull)_;
  @objc public func test2(_: [String: any P & Sendable]) {}
}

@objc public protocol Q {
  // CHECK: - (NSArray<NSDictionary<NSString *, id> *> * _Nonnull)data1 SWIFT_WARN_UNUSED_RESULT;
  func data1() -> [[String: any Sendable]]
  // CHECK: - (NSArray * _Nullable)data2 SWIFT_WARN_UNUSED_RESULT;
  func data2() -> [any Sendable]?
  // CHECK: - (void)data3:(id _Nonnull)_;
  func data3(_: any Sendable)
  // CHECK: - (void)data4:(id _Nullable)_;
  func data4(_: (any Sendable)?)
}
