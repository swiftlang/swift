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

public protocol Q {
  @available(macOS 10.15, iOS 13.0, watchOS 6.0, tvOS 13.0, *)
  associatedtype AT = Int

  @available(macOS 10.15, iOS 13.0, watchOS 6.0, tvOS 13.0, *)
  func f() -> AT
}

// CHECK: @available(anyAppleOS 27, *)
// CHECK-NEXT: public struct InferredWitnessInAnyAppleOS : assoc::Q {
@available(anyAppleOS 27, *)
public struct InferredWitnessInAnyAppleOS: Q {
  public func f() -> String { "" }

  // The inferred type witness must not be more available than the struct.
  // CHECK: @available(anyAppleOS 27, macOS 27, iOS 27, tvOS 27, watchOS 27, *)
  // CHECK-NEXT: public typealias AT = Swift::String
}
