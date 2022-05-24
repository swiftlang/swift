// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name main
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name main
// RUN: %FileCheck %s < %t.swiftinterface

// Verify that `any` is not required in swiftinterfaces.
// RUN: %target-swift-typecheck-module-from-interface(%S/Inputs/existential-any-ignore-missing-in-interface.swiftinterface) -module-name ExistentialAnyMissing

// CHECK: public protocol P
public protocol P { }

// CHECK: public protocol Q
public protocol Q {
  // CHECK: associatedtype A : main.P
  associatedtype A: P
}

// CHECK: public func takesAndReturnsP(_ x: any main.P) -> any main.P
public func takesAndReturnsP(_ x: P) -> P {
  return x
}

// CHECK: public func takesAndReturnsOptionalP(_ x: (any main.P)?) -> (any main.P)?
public func takesAndReturnsOptionalP(_ x: P?) -> P? {
  return x
}

// CHECK: public func takesAndReturnsQ(_ x: any main.Q) -> any main.Q
public func takesAndReturnsQ(_ x: any Q) -> any Q {
  return x
}

// CHECK: public struct S
public struct S {
  // CHECK: public var p: any main.P
  public var p: P
  // CHECK: public var maybeP: (any main.P)?
  public var maybeP: P?
  // CHECK: public var q: any main.Q
  public var q: any Q
}


public protocol ProtocolTypealias {
  typealias A = P
}

// CHECK: public func dependentExistential<T>(value: (T) -> any main.P) where T : main.ProtocolTypealias
public func dependentExistential<T: ProtocolTypealias>(value: (T) -> T.A) {}
