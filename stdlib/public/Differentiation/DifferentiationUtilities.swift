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
