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
// Differentiable function creation
//===----------------------------------------------------------------------===//

/// Create a differentiable function from a vector-Jacobian products function.
@inlinable
public func differentiableFunction<T : Differentiable, R : Differentiable>(
  from vjp: @escaping (T)
           -> (value: R, pullback: (R.TangentVector) -> T.TangentVector)
) -> @differentiable (T) -> R {
  Builtin.differentiableFunction_arity1(
    /*original*/ { vjp($0).value },
    /*jvp*/ { _ in
      fatalError("""
        Functions formed with `differentiableFunction(from:)` cannot yet \
        be used with differential-producing differential operators.
        """)
    },
    /*vjp*/ vjp)
}

/// Create a differentiable function from a vector-Jacobian products function.
@inlinable
public func differentiableFunction<T, U, R>(
  from vjp: @escaping (T, U)
           -> (value: R, pullback: (R.TangentVector)
             -> (T.TangentVector, U.TangentVector))
) -> @differentiable (T, U) -> R {
  Builtin.differentiableFunction_arity2(
    /*original*/ { vjp($0, $1).value },
    /*jvp*/ { _, _ in
      fatalError("""
        Functions formed with `differentiableFunction(from:)` cannot yet \
        be used with differential-producing differential operators.
        """)
    },
    /*vjp*/ vjp)
}

/// Create a differentiable function from a vector-Jacobian products function.
@inlinable
public func differentiableFunction<T, U, V, R>(
  from vjp: @escaping (T, U, V)
           -> (value: R, pullback: (R.TangentVector)
             -> (T.TangentVector, U.TangentVector, V.TangentVector))
) -> @differentiable (T, U, V) -> R {
  Builtin.differentiableFunction_arity3(
    /*original*/ { vjp($0, $1, $2).value },
    /*jvp*/ { _, _, _ in
      fatalError("""
        Functions formed with `differentiableFunction(from:)` cannot yet \
        be used with differential-producing differential operators.
        """)
    },
    /*vjp*/ vjp)
}

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
  @differentiable(wrt: self)
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
