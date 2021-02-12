//===--- DifferentiationUtilities.swift -----------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Utilities for creating differentiable functions, debugging, and customizing
// derivatives.
//
//===----------------------------------------------------------------------===//

import Swift

//===----------------------------------------------------------------------===//
// Derivative customization
//===----------------------------------------------------------------------===//

/// Returns `x` like an identity function. When used in a context where `x` is
/// being differentiated with respect to, this function will not produce any 
/// derivative at `x`.
@inlinable
@inline(__always)
@_semantics("autodiff.nonvarying")
public func withoutDerivative<T>(at x: T) -> T {
  x
}

/// Applies the given closure `body` to `x`. When used in a context where `x` is
/// being differentiated with respect to, this function will not produce any
/// derivative at `x`.
// FIXME: Support throws-rethrows.
@inlinable
@inline(__always)
@_semantics("autodiff.nonvarying")
public func withoutDerivative<T, R>(at x: T, in body: (T) -> R) -> R {
  body(x)
}

public extension Differentiable {
  /// Applies the given closure to the derivative of `self`.
  ///
  /// Returns `self` like an identity function. When the return value is used in
  /// a context where it is differentiated with respect to, applies the given
  /// closure to the derivative of the return value.
  @inlinable
  @differentiable(reverse, wrt: self)
  func withDerivative(_ body: @escaping (inout TangentVector) -> Void) -> Self {
    return self
  }

  @inlinable
  @derivative(of: withDerivative)
  internal func _vjpWithDerivative(
    _ body: @escaping (inout TangentVector) -> Void
  ) -> (value: Self, pullback: (TangentVector) -> TangentVector) {
    return (self, { grad in
      var grad = grad
      body(&grad)
      return grad
    })
  }
}

//===----------------------------------------------------------------------===//
// Diagnostics
//===----------------------------------------------------------------------===//

@_silgen_name("_fatalErrorForwardModeDifferentiationDisabled")
public func _fatalErrorForwardModeDifferentiationDisabled() -> Never {
  fatalError("""
    JVP does not exist. Use \
    '-Xfrontend -enable-experimental-forward-mode-differentiation' to enable \
    differential-first differentiation APIs.
    """)
}
