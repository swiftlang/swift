// RUN: %target-swift-frontend-typecheck -swift-version 5 -enable-library-evolution -emit-module-interface-path %t.swiftinterface %s -module-name main
// RUN: %target-swift-frontend -typecheck-module-from-interface %t.swiftinterface -module-name main
// RUN: %FileCheck %s < %t.swiftinterface

// Verify that `any` is not required in swiftinterfaces.
// RUN: %target-swift-frontend -typecheck-module-from-interface %S/Inputs/existential-any-ignore-missing-in-interface.swiftinterface -module-name ExistentialAnyMissing

// CHECK: public protocol P
public protocol P { }

// CHECK: public protocol Q
public protocol Q {
  // CHECK: associatedtype A : main.P
  associatedtype A: P
}

// CHECK: public func takesAndReturnsP(_ x: main.P) -> main.P
public func takesAndReturnsP(_ x: P) -> P {
  return x
}

// CHECK: public func takesAndReturnsOptionalP(_ x: main.P?) -> main.P?
public func takesAndReturnsOptionalP(_ x: P?) -> P? {
  return x
}

// CHECK: public func takesAndReturnsQ(_ x: main.Q) -> main.Q
public func takesAndReturnsQ(_ x: any Q) -> any Q {
  return x
}

// CHECK: public struct S
public struct S {
  // CHECK: public var p: main.P
  public var p: P
  // CHECK: public var maybeP: main.P?
  public var maybeP: P?
  // CHECK: public var q: main.Q
  public var q: any Q
}


public protocol ProtocolTypealias {
  typealias A = P
}

// CHECK: public func dependentExistential<T>(value: (T) -> T.A) where T : main.ProtocolTypealias
public func dependentExistential<T: ProtocolTypealias>(value: (T) -> T.A) {}

public typealias Composition = ProtocolTypealias & P

// CHECK: public func optionalComposition(value: main.Composition?)
public func optionalComposition(value: Composition?) {}
