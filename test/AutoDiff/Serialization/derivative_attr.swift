// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-differentiable-programming %s -emit-module -parse-as-library -o %t
// RUN: llvm-bcanalyzer %t/derivative_attr.swiftmodule | %FileCheck %s -check-prefix=BCANALYZER
// RUN: %target-sil-opt -enable-experimental-differentiable-programming -disable-sil-linking -enable-sil-verify-all %t/derivative_attr.swiftmodule -o - | %FileCheck %s

// BCANALYZER-NOT: UnknownCode

// REQUIRES: differentiable_programming

import _Differentiation

// Dummy `Differentiable`-conforming type.
struct S: Differentiable & AdditiveArithmetic {
  static var zero: S { S() }
  static func + (_: S, _: S) -> S { S() }
  static func - (_: S, _: S) -> S { S() }
  typealias TangentVector = S
}

// Test top-level functions.

func top1(_ x: S) -> S {
  x
}
// CHECK: @derivative(of: top1, wrt: x)
@derivative(of: top1, wrt: x)
func derivativeTop1(_ x: S) -> (value: S, differential: (S) -> S) {
  (x, { $0 })
}

func top2<T, U>(_ x: T, _ i: Int, _ y: U) -> U {
  y
}
// CHECK: @derivative(of: top2, wrt: (x, y))
@derivative(of: top2, wrt: (x, y))
func derivativeTop2<T: Differentiable, U: Differentiable>(
  _ x: T, _ i: Int, _ y: U
) -> (value: U, differential: (T.TangentVector, U.TangentVector) -> U.TangentVector) {
  (y, { (dx, dy) in dy })
}

// Test instance methods.

extension S {
  func instanceMethod(_ x: S) -> S {
    self
  }

  // CHECK: @derivative(of: instanceMethod, wrt: x)
  @derivative(of: instanceMethod, wrt: x)
  func derivativeInstanceMethodWrtX(_ x: S) -> (value: S, differential: (S) -> S) {
    (self, { _ in .zero })
  }

  // CHECK: @derivative(of: instanceMethod, wrt: self)
  @derivative(of: instanceMethod, wrt: self)
  func derivativeInstanceMethodWrtSelf(_ x: S) -> (value: S, differential: (S) -> S) {
    (self, { $0 })
  }

  // CHECK: @derivative(of: instanceMethod, wrt: (self, x))
  @derivative(of: instanceMethod, wrt: (self, x))
  func derivativeInstanceMethodWrtAll(_ x: S) -> (value: S, differential: (S, S) -> S) {
    (self, { (dself, dx) in self })
  }
}

// Test static methods.

extension S {
  static func staticMethod(_ x: S) -> S {
    x
  }

  // CHECK: @derivative(of: staticMethod, wrt: x)
  @derivative(of: staticMethod, wrt: x)
  static func derivativeStaticMethod(_ x: S) -> (value: S, differential: (S) -> S) {
    (x, { $0 })
  }
}

// Test computed properties.

extension S {
  var computedProperty: S {
    self
  }

  // CHECK: @derivative(of: computedProperty, wrt: self)
  @derivative(of: computedProperty, wrt: self)
  func derivativeProperty() -> (value: S, differential: (S) -> S) {
    (self, { $0 })
  }
}

// Test subscripts.

extension S {
  subscript<T: Differentiable>(x: T) -> S {
    self
  }

  // CHECK: @derivative(of: subscript, wrt: self)
  @derivative(of: subscript(_:), wrt: self)
  func derivativeSubscript<T: Differentiable>(x: T) -> (value: S, differential: (S) -> S) {
    (self, { $0 })
  }
}
