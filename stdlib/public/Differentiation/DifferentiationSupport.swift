//===--- DifferentiationSupport.swift -------------------------*- swift -*-===//
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
// This file defines support for differentiable programming APIs.
//
//===----------------------------------------------------------------------===//

import Swift

//===----------------------------------------------------------------------===//
// Functional utilities
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

//===----------------------------------------------------------------------===//
// Free-function-style differential operators
//===----------------------------------------------------------------------===//

// Value with differential

@inlinable
public func valueWithDifferential<T, R>(
  at x: T, in f: @differentiable (T) -> R
) -> (value: R, differential: (T.TangentVector) -> R.TangentVector) {
  return Builtin.applyDerivative_jvp(f, x)
}

@inlinable
public func valueWithDifferential<T, U, R>(
  at x: T, _ y: U, in f: @differentiable (T, U) -> R
) -> (value: R,
      differential: (T.TangentVector, U.TangentVector) -> R.TangentVector) {
  return Builtin.applyDerivative_jvp_arity2(f, x, y)
}

@inlinable
public func valueWithDifferential<T, U, V, R>(
  at x: T, _ y: U, _ z: V, in f: @differentiable (T, U, V) -> R
) -> (value: R,
      differential: (T.TangentVector, U.TangentVector, V.TangentVector)
        -> (R.TangentVector)) {
  return Builtin.applyDerivative_jvp_arity3(f, x, y, z)
}

// Value with pullback

@inlinable
public func valueWithPullback<T, R>(
  at x: T, in f: @differentiable (T) -> R
) -> (value: R, pullback: (R.TangentVector) -> T.TangentVector) {
  return Builtin.applyDerivative_vjp(f, x)
}

@inlinable
public func valueWithPullback<T, U, R>(
  at x: T, _ y: U, in f: @differentiable (T, U) -> R
) -> (value: R,
      pullback: (R.TangentVector) -> (T.TangentVector, U.TangentVector)) {
  return Builtin.applyDerivative_vjp_arity2(f, x, y)
}

@inlinable
public func valueWithPullback<T, U, V, R>(
  at x: T, _ y: U, _ z: V, in f: @differentiable (T, U, V) -> R
) -> (value: R,
      pullback: (R.TangentVector)
        -> (T.TangentVector, U.TangentVector, V.TangentVector)) {
  return Builtin.applyDerivative_vjp_arity3(f, x, y, z)
}

// Differential

@inlinable
public func differential<T, R>(
  at x: T, in f: @differentiable (T) -> R
) -> (T.TangentVector) -> R.TangentVector {
  return valueWithDifferential(at: x, in: f).1
}

@inlinable
public func differential<T, U, R>(
  at x: T, _ y: U, in f: @differentiable (T, U) -> R
) -> (T.TangentVector, U.TangentVector) -> R.TangentVector {
  return valueWithDifferential(at: x, y, in: f).1
}

@inlinable
public func differential<T, U, V, R>(
  at x: T, _ y: U, _ z: V, in f: @differentiable (T, U, V) -> R
) -> (T.TangentVector, U.TangentVector, V.TangentVector) -> (R.TangentVector) {
  return valueWithDifferential(at: x, y, z, in: f).1
}

// Pullback

@inlinable
public func pullback<T, R>(
  at x: T, in f: @differentiable (T) -> R
) -> (R.TangentVector) -> T.TangentVector {
  return Builtin.applyDerivative_vjp(f, x).1
}

@inlinable
public func pullback<T, U, R>(
  at x: T, _ y: U, in f: @differentiable (T, U) -> R
) -> (R.TangentVector) -> (T.TangentVector, U.TangentVector) {
  return Builtin.applyDerivative_vjp_arity2(f, x, y).1
}

@inlinable
public func pullback<T, U, V, R>(
  at x: T, _ y: U, _ z: V, in f: @differentiable (T, U, V) -> R
) -> (R.TangentVector)
    -> (T.TangentVector, U.TangentVector, V.TangentVector) {
  return Builtin.applyDerivative_vjp_arity3(f, x, y, z).1
}

// Derivative

@inlinable
public func derivative<T: FloatingPoint, R>(
  at x: T, in f: @differentiable (T) -> R
) ->  R.TangentVector
  where T.TangentVector == T {
  return differential(at: x, in: f)(T(1))
}

@inlinable
public func derivative<T: FloatingPoint, U: FloatingPoint, R>(
  at x: T, _ y: U, in f: @differentiable (T, U) -> R
) -> R.TangentVector
  where T.TangentVector == T,
        U.TangentVector == U {
  return differential(at: x, y, in: f)(T(1), U(1))
}

@inlinable
public func derivative<T: FloatingPoint, U: FloatingPoint, V: FloatingPoint, R>(
  at x: T, _ y: U, _ z: V, in f: @differentiable (T, U, V) -> R
) -> R.TangentVector
  where T.TangentVector == T,
        U.TangentVector == U,
        V.TangentVector == V {
  return differential(at: x, y, z, in: f)(T(1), U(1), V(1))
}

// Gradient

@inlinable
public func gradient<T, R>(
  at x: T, in f: @differentiable (T) -> R
) -> T.TangentVector
  where R : FloatingPoint, R.TangentVector == R {
  return pullback(at: x, in: f)(R(1))
}

@inlinable
public func gradient<T, U, R>(
  at x: T, _ y: U, in f: @differentiable (T, U) -> R
) -> (T.TangentVector, U.TangentVector)
  where R : FloatingPoint, R.TangentVector == R {
  return pullback(at: x, y, in: f)(R(1))
}

@inlinable
public func gradient<T, U, V, R>(
  at x: T, _ y: U, _ z: V, in f: @differentiable (T, U, V) -> R
) -> (T.TangentVector, U.TangentVector, V.TangentVector)
  where R : FloatingPoint, R.TangentVector == R {
  return pullback(at: x, y, z, in: f)(R(1))
}

// Value with derivative

@inlinable
public func valueWithDerivative<T: FloatingPoint, R>(
  at x: T, in f: @differentiable (T) -> R
) -> (value: R, derivative: R.TangentVector)
  where T.TangentVector == T {
  let (y, differential) = valueWithDifferential(at: x, in: f)
  return (y, differential(T(1)))
}

@inlinable
public func valueWithDerivative<T: FloatingPoint, U: FloatingPoint, R>(
  at x: T, _ y: U, in f: @differentiable (T, U) -> R
) -> (value: R, derivative: R.TangentVector)
  where T.TangentVector == T,
        U.TangentVector == U {
  let (y, differential) = valueWithDifferential(at: x, y, in: f)
  return (y, differential(T(1), U(1)))
}

@inlinable
public func valueWithDerivative<
  T: FloatingPoint, U: FloatingPoint, V: FloatingPoint, R>(
  at x: T, _ y: U, _ z: V, in f: @differentiable (T, U, V) -> R
) -> (value: R, derivative: R.TangentVector)
  where T.TangentVector == T,
        U.TangentVector == U,
        V.TangentVector == V {
  let (y, differential) = valueWithDifferential(at: x, y, z, in: f)
  return (y, differential(T(1), U(1), V(1)))
}

// Value with gradient

@inlinable
public func valueWithGradient<T, R>(
  at x: T, in f: @differentiable (T) -> R
) -> (value: R, gradient: T.TangentVector)
  where R : FloatingPoint, R.TangentVector == R {
  let (y, pullback) = valueWithPullback(at: x, in: f)
  return (y, pullback(R(1)))
}

@inlinable
public func valueWithGradient<T, U, R>(
  at x: T, _ y: U, in f: @differentiable (T, U) -> R
) -> (value: R, gradient: (T.TangentVector, U.TangentVector))
  where R : FloatingPoint, R.TangentVector == R {
  let (y, pullback) = valueWithPullback(at: x, y, in: f)
  return (y, pullback(R(1)))
}

@inlinable
public func valueWithGradient<T, U, V, R>(
  at x: T, _ y: U, _ z: V, in f: @differentiable (T, U, V) -> R
) -> (value: R,
      gradient: (T.TangentVector, U.TangentVector, V.TangentVector))
  where R : FloatingPoint, R.TangentVector == R {
  let (y, pullback) = valueWithPullback(at: x, y, z, in: f)
  return (y, pullback(R(1)))
}
