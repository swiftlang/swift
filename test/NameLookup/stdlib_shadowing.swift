// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/HasResult.swift -enable-library-evolution -swift-version 5
// RUN: %target-swift-frontend -typecheck %s -I %t -verify -enable-library-evolution -emit-module-interface-path %t/stdlib_shadowing.swiftinterface -swift-version 5

// Since HasResult is right here, let's make sure we emit interfaces correctly.
// RUN: %FileCheck %s < %t/stdlib_shadowing.swiftinterface

import HasResult

public func foo() -> Result<Int, Error> {
  return Result<Int, Error>.success(42)
}
// CHECK: public func foo() -> HasResult.Result

extension Result {
  public func library() {}
}
// CHECK: extension HasResult.Result {
// CHECK-NEXT: public func library()
// CHECK-NEXT: }

extension Swift.Result {
  public func stdlib() {}
}
// CHECK: extension Swift.Result {
// CHECK-NEXT: public func stdlib()
// CHECK-NEXT: }
