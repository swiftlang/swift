// Verify that we continue printing parentheses in module interfaces even if
// they weren't present in source.
//
// RUN: %target-swift-emit-module-interface(%t.swiftinterface) -module-name SomeAnyOptionalSugar %s
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name SomeAnyOptionalSugar
// RUN: %FileCheck %s < %t.swiftinterface

public protocol P {
  typealias R = Q
}
public protocol Q {}

// CHECK: public func f(_ x: (some P)?) -> (any SomeAnyOptionalSugar::P)?
public func f(_ x: some P?) -> any P? {}

// CHECK: public func g(_ x: borrowing (any ~Copyable)?)
public func g(_ x: borrowing any ~Copyable?) {}

// CHECK: public func h(_ x: (some P.R)?)
public func h(_ x: some P.R?) {}
