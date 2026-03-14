// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name assoc
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name assoc
// RUN: %FileCheck %s < %t.swiftinterface

// REQUIRES: concurrency, objc_interop

// CHECK: public protocol P
public protocol P {
  // CHECK: @available(macOS 10.15, iOS 13.0, watchOS 6.0, tvOS 13.0, *)
  // CHECK-NEXT: associatedtype AT = Self
  @available(macOS 10.15, iOS 13.0, watchOS 6.0, tvOS 13.0, *)
  associatedtype AT = Self
}
