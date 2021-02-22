//===--- DifferentialOperators.swift --------------------------*- swift -*-===//
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
// APIs for computing derivatives of functions.
//
//===----------------------------------------------------------------------===//

import Swift

// Value with pullback

@inlinable
public func valueWithPullback<T, R>(
  at x: T, in f: @differentiable(reverse) (T) -> R
) -> (value: R, pullback: (R.TangentVector) -> T.TangentVector) {
  return Builtin.applyDerivative_vjp(f, x)
}

@inlinable
public func valueWithPullback<T, U, R>(
  at x: T, _ y: U, in f: @differentiable(reverse) (T, U) -> R
) -> (value: R,
      pullback: (R.TangentVector) -> (T.TangentVector, U.TangentVector)) {
  return Builtin.applyDerivative_vjp_arity2(f, x, y)
}

@inlinable
public func valueWithPullback<T, U, V, R>(
  at x: T, _ y: U, _ z: V, in f: @differentiable(reverse) (T, U, V) -> R
) -> (value: R,
      pullback: (R.TangentVector)
        -> (T.TangentVector, U.TangentVector, V.TangentVector)) {
  return Builtin.applyDerivative_vjp_arity3(f, x, y, z)
}

// Pullback

@inlinable
public func pullback<T, R>(
  at x: T, in f: @differentiable(reverse) (T) -> R
) -> (R.TangentVector) -> T.TangentVector {
  return Builtin.applyDerivative_vjp(f, x).1
}

@inlinable
public func pullback<T, U, R>(
  at x: T, _ y: U, in f: @differentiable(reverse) (T, U) -> R
) -> (R.TangentVector) -> (T.TangentVector, U.TangentVector) {
  return Builtin.applyDerivative_vjp_arity2(f, x, y).1
}

@inlinable
public func pullback<T, U, V, R>(
  at x: T, _ y: U, _ z: V, in f: @differentiable(reverse) (T, U, V) -> R
) -> (R.TangentVector)
    -> (T.TangentVector, U.TangentVector, V.TangentVector) {
  return Builtin.applyDerivative_vjp_arity3(f, x, y, z).1
}

// Gradient

@inlinable
public func gradient<T, R>(
  at x: T, in f: @differentiable(reverse) (T) -> R
) -> T.TangentVector
  where R : FloatingPoint, R.TangentVector == R {
  return pullback(at: x, in: f)(R(1))
}

@inlinable
public func gradient<T, U, R>(
  at x: T, _ y: U, in f: @differentiable(reverse) (T, U) -> R
) -> (T.TangentVector, U.TangentVector)
  where R : FloatingPoint, R.TangentVector == R {
  return pullback(at: x, y, in: f)(R(1))
}

@inlinable
public func gradient<T, U, V, R>(
  at x: T, _ y: U, _ z: V, in f: @differentiable(reverse) (T, U, V) -> R
) -> (T.TangentVector, U.TangentVector, V.TangentVector)
  where R : FloatingPoint, R.TangentVector == R {
  return pullback(at: x, y, z, in: f)(R(1))
}

// Value with gradient

@inlinable
public func valueWithGradient<T, R>(
  at x: T, in f: @differentiable(reverse) (T) -> R
) -> (value: R, gradient: T.TangentVector)
  where R : FloatingPoint, R.TangentVector == R {
  let (y, pullback) = valueWithPullback(at: x, in: f)
  return (y, pullback(R(1)))
}

@inlinable
public func valueWithGradient<T, U, R>(
  at x: T, _ y: U, in f: @differentiable(reverse) (T, U) -> R
) -> (value: R, gradient: (T.TangentVector, U.TangentVector))
  where R : FloatingPoint, R.TangentVector == R {
  let (y, pullback) = valueWithPullback(at: x, y, in: f)
  return (y, pullback(R(1)))
}

@inlinable
public func valueWithGradient<T, U, V, R>(
  at x: T, _ y: U, _ z: V, in f: @differentiable(reverse) (T, U, V) -> R
) -> (value: R,
      gradient: (T.TangentVector, U.TangentVector, V.TangentVector))
  where R : FloatingPoint, R.TangentVector == R {
  let (y, pullback) = valueWithPullback(at: x, y, z, in: f)
  return (y, pullback(R(1)))
}

// Gradient (curried)

@inlinable
public func gradient<T, R>(
  of f: @escaping @differentiable(reverse) (T) -> R
) -> (T) -> T.TangentVector
  where R : FloatingPoint, R.TangentVector == R {
  return { x in gradient(at: x, in: f) }
}

@inlinable
public func gradient<T, U, R>(
  of f: @escaping @differentiable(reverse) (T, U) -> R
) -> (T, U) -> (T.TangentVector, U.TangentVector)
  where R : FloatingPoint, R.TangentVector == R {
  return { x, y in gradient(at: x, y, in: f) }
}

@inlinable
public func gradient<T, U, V, R>(
  of f: @escaping @differentiable(reverse) (T, U, V) -> R
) -> (T, U, V) -> (T.TangentVector, U.TangentVector, V.TangentVector)
  where R : FloatingPoint, R.TangentVector == R {
  return { x, y, z in gradient(at: x, y, z, in: f) }
}
