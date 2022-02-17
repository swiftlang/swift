// RUN: %empty-directory(%t)
// RUN: not --crash %target-build-swift -emit-module -module-name sr15849 -emit-module-path %t/sr15849.swiftmodule -swift-version 5 -c %S/sr15849-mutating-functions-with-control-flow.swift
// XFAIL: *

// SR-15849: Mutating functions with control flow can cause assertion failure
// for conflicting debug variable type.

import _Differentiation

// Declare `Tensor`

class TensorHandle {} // Crash requires `class` and not `struct`

struct Tensor<Scalar> {
  let handle: TensorHandle
}

extension Tensor: Differentiable {
  typealias TangentVector = Tensor
}

extension Tensor: AdditiveArithmetic  {
  static func == (lhs: Tensor, rhs: Tensor) -> Bool { fatalError() }
  static func != (lhs: Tensor, rhs: Tensor) -> Bool { fatalError() }
  
  static var zero: Tensor { fatalError() }

  @differentiable(reverse)
  static func + (lhs: Tensor, rhs: Tensor) -> Tensor { fatalError() }
  static func - (lhs: Tensor, rhs: Tensor) -> Tensor { fatalError() }
}

// Make `+=` differentiable

extension Tensor {
  @derivative(of: +)
  static func _vjpAdd(lhs: Tensor, rhs: Tensor) -> (
    value: Tensor, pullback: (Tensor) -> (Tensor, Tensor)
  ) {
    fatalError()
  }
  
  static func += (lhs: inout Tensor, rhs: Tensor) {
    lhs = lhs + rhs
  }
}

// Declare `BatchNorm`

protocol Layer: Differentiable {
  associatedtype Input
  associatedtype Output: Differentiable

  @differentiable(reverse)
  func callAsFunction(_ input: Input) -> Output
}

struct BatchNorm<Scalar>: Layer { // Crash requires conformance to `Layer`
  @noDerivative let momentum: Scalar // Crash requires `@noDerivative`
  var offset: Tensor<Scalar>

  @differentiable(reverse)
  func callAsFunction(_ input: Tensor<Scalar>) -> Tensor<Scalar> {
    var offset = self.offset
    if true { // Crash requires `if true`
      offset += offset // Using `offset = offset + offset` stops the crash
    }
    return offset
  }
}
