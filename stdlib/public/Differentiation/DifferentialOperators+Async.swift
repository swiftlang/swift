//===--- DifferentialOperators.swift --------------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// APIs for computing derivatives of async functions.
//
//===----------------------------------------------------------------------===//

import Swift
import _Concurrency

// Value with differential

@inlinable
@available(StdlibDeploymentTarget 6.5, *)
public func valueWithDifferential<T, R>(
  at x: T, of f: @differentiable(reverse) (T) async -> R
) async -> (value: R, differential: (T.TangentVector) -> R.TangentVector) {
  return await Builtin.applyDerivative_jvp_async(f, x)
}

@inlinable
@available(StdlibDeploymentTarget 6.5, *)
public func valueWithDifferential<T, U, R>(
  at x: T, _ y: U, of f: @differentiable(reverse) (T, U) async -> R
) async -> (value: R,
      differential: (T.TangentVector, U.TangentVector) -> R.TangentVector) {
  return await Builtin.applyDerivative_jvp_arity2_async(f, x, y)
}

@inlinable
@available(StdlibDeploymentTarget 6.5, *)
public func valueWithDifferential<T, U, V, R>(
  at x: T, _ y: U, _ z: V, of f: @differentiable(reverse) (T, U, V) async -> R
) async -> (value: R,
      differential: (T.TangentVector, U.TangentVector, V.TangentVector)
        -> (R.TangentVector)) {
  return await Builtin.applyDerivative_jvp_arity3_async(f, x, y, z)
}


// Value with pullback

@inlinable
@available(StdlibDeploymentTarget 6.5, *)
public func valueWithPullback<T, R>(
  at x: T, of f: @differentiable(reverse) (T) async -> R
) async -> (value: R, pullback: (R.TangentVector) -> T.TangentVector) {
  return await Builtin.applyDerivative_vjp_async(f, x)
}

@inlinable
@available(StdlibDeploymentTarget 6.5, *)
public func valueWithPullback<T, U, R>(
  at x: T, _ y: U, of f: @differentiable(reverse) (T, U) async -> R
) async -> (value: R,
      pullback: (R.TangentVector) -> (T.TangentVector, U.TangentVector)) {
  return await Builtin.applyDerivative_vjp_arity2_async(f, x, y)
}

@inlinable
@available(StdlibDeploymentTarget 6.5, *)
public func valueWithPullback<T, U, V, R>(
  at x: T, _ y: U, _ z: V, of f: @differentiable(reverse) (T, U, V) async -> R
) async -> (value: R,
      pullback: (R.TangentVector)
        -> (T.TangentVector, U.TangentVector, V.TangentVector)) {
  return await Builtin.applyDerivative_vjp_arity3_async(f, x, y, z)
}

// Differential

@inlinable
@available(StdlibDeploymentTarget 6.5, *)
public func differential<T, R>(
  at x: T, of f: @differentiable(reverse) (T) async -> R
) async -> (T.TangentVector) -> R.TangentVector {
  return await valueWithDifferential(at: x, of: f).1
}

@inlinable
@available(StdlibDeploymentTarget 6.5, *)
public func differential<T, U, R>(
  at x: T, _ y: U, of f: @differentiable(reverse) (T, U) async -> R
) async -> (T.TangentVector, U.TangentVector) -> R.TangentVector {
  return await valueWithDifferential(at: x, y, of: f).1
}

@inlinable
@available(StdlibDeploymentTarget 6.5, *)
public func differential<T, U, V, R>(
  at x: T, _ y: U, _ z: V, of f: @differentiable(reverse) (T, U, V) async -> R
) async -> (T.TangentVector, U.TangentVector, V.TangentVector) -> (R.TangentVector) {
  return await valueWithDifferential(at: x, y, z, of: f).1
}

// Pullback

@inlinable
@available(StdlibDeploymentTarget 6.5, *)
public func pullback<T, R>(
  at x: T, of f: @differentiable(reverse) (T) async -> R
) async -> (R.TangentVector) -> T.TangentVector {
  return await Builtin.applyDerivative_vjp_async(f, x).1
}

@inlinable
@available(StdlibDeploymentTarget 6.5, *)
public func pullback<T, U, R>(
  at x: T, _ y: U, of f: @differentiable(reverse) (T, U) async -> R
) async -> (R.TangentVector) -> (T.TangentVector, U.TangentVector) {
  return await Builtin.applyDerivative_vjp_arity2_async(f, x, y).1
}

@inlinable
@available(StdlibDeploymentTarget 6.5, *)
public func pullback<T, U, V, R>(
  at x: T, _ y: U, _ z: V, of f: @differentiable(reverse) (T, U, V) async -> R
) async -> (R.TangentVector)
    -> (T.TangentVector, U.TangentVector, V.TangentVector) {
  return await Builtin.applyDerivative_vjp_arity3_async(f, x, y, z).1
}

// Derivative

@inlinable
@available(StdlibDeploymentTarget 6.5, *)
public func derivative<T: FloatingPoint, R>(
  at x: T, of f: @differentiable(reverse) (T) async -> R
) async ->  R.TangentVector
  where T.TangentVector == T {
  return await differential(at: x, of: f)(T(1))
}

@inlinable
@available(StdlibDeploymentTarget 6.5, *)
public func derivative<T: FloatingPoint, U: FloatingPoint, R>(
  at x: T, _ y: U, of f: @differentiable(reverse) (T, U) async -> R
) async -> R.TangentVector
  where T.TangentVector == T,
        U.TangentVector == U {
  return await differential(at: x, y, of: f)(T(1), U(1))
}

@inlinable
@available(StdlibDeploymentTarget 6.5, *)
public func derivative<T: FloatingPoint, U: FloatingPoint, V: FloatingPoint, R>(
  at x: T, _ y: U, _ z: V, of f: @differentiable(reverse) (T, U, V) async -> R
) async -> R.TangentVector
  where T.TangentVector == T,
        U.TangentVector == U,
        V.TangentVector == V {
  return await differential(at: x, y, z, of: f)(T(1), U(1), V(1))
}

// Gradient

@inlinable
@available(StdlibDeploymentTarget 6.5, *)
public func gradient<T, R>(
  at x: T, of f: @differentiable(reverse) (T) async -> R
) async -> T.TangentVector
  where R : FloatingPoint, R.TangentVector == R {
  return await pullback(at: x, of: f)(R(1))
}

@inlinable
@available(StdlibDeploymentTarget 6.5, *)
public func gradient<T, U, R>(
  at x: T, _ y: U, of f: @differentiable(reverse) (T, U) async -> R
) async -> (T.TangentVector, U.TangentVector)
  where R : FloatingPoint, R.TangentVector == R {
  return await pullback(at: x, y, of: f)(R(1))
}

@inlinable
@available(StdlibDeploymentTarget 6.5, *)
public func gradient<T, U, V, R>(
  at x: T, _ y: U, _ z: V, of f: @differentiable(reverse) (T, U, V) async -> R
) async -> (T.TangentVector, U.TangentVector, V.TangentVector)
  where R : FloatingPoint, R.TangentVector == R {
  return await pullback(at: x, y, z, of: f)(R(1))
}

// Value with derivative

@inlinable
@available(StdlibDeploymentTarget 6.5, *)
public func valueWithDerivative<T: FloatingPoint, R>(
  at x: T, of f: @escaping @differentiable(reverse) (T) async -> R
) async -> (value: R, derivative: R.TangentVector)
  where T.TangentVector == T {
  let (y, differential) = await valueWithDifferential(at: x, of: f)
  return (y, differential(T(1)))
}

@inlinable
@available(StdlibDeploymentTarget 6.5, *)
public func valueWithDerivative<T: FloatingPoint, U: FloatingPoint, R>(
  at x: T, _ y: U, of f: @escaping @differentiable(reverse) (T, U) async -> R
) async -> (value: R, derivative: R.TangentVector)
  where T.TangentVector == T,
        U.TangentVector == U {
  let (y, differential) = await valueWithDifferential(at: x, y, of: f)
  return (y, differential(T(1), U(1)))
}

@inlinable
@available(StdlibDeploymentTarget 6.5, *)
public func valueWithDerivative<
  T: FloatingPoint, U: FloatingPoint, V: FloatingPoint, R>(
  at x: T, _ y: U, _ z: V, of f: @escaping @differentiable(reverse) (T, U, V) async -> R
) async -> (value: R, derivative: R.TangentVector)
  where T.TangentVector == T,
        U.TangentVector == U,
        V.TangentVector == V {
  let (y, differential) = await valueWithDifferential(at: x, y, z, of: f)
  return (y, differential(T(1), U(1), V(1)))
}

// Value with gradient

@inlinable
@available(StdlibDeploymentTarget 6.5, *)
public func valueWithGradient<T, R>(
  at x: T, of f: @differentiable(reverse) (T) async -> R
) async -> (value: R, gradient: T.TangentVector)
  where R : FloatingPoint, R.TangentVector == R {
  let (y, pullback) = await valueWithPullback(at: x, of: f)
  return (y,  pullback(R(1)))
}

@inlinable
@available(StdlibDeploymentTarget 6.5, *)
public func valueWithGradient<T, U, R>(
  at x: T, _ y: U, of f: @differentiable(reverse) (T, U) async -> R
) async -> (value: R, gradient: (T.TangentVector, U.TangentVector))
  where R : FloatingPoint, R.TangentVector == R {
  let (y, pullback) = await valueWithPullback(at: x, y, of: f)
  return (y, pullback(R(1)))
}

@inlinable
@available(StdlibDeploymentTarget 6.5, *)
public func valueWithGradient<T, U, V, R>(
  at x: T, _ y: U, _ z: V, of f: @differentiable(reverse) (T, U, V) async -> R
) async -> (value: R,
      gradient: (T.TangentVector, U.TangentVector, V.TangentVector))
  where R : FloatingPoint, R.TangentVector == R {
  let (y, pullback) = await valueWithPullback(at: x, y, z, of: f)
  return (y, pullback(R(1)))
}

// Derivative (curried)

@inlinable
@available(StdlibDeploymentTarget 6.5, *)
public func derivative<T: FloatingPoint, R>(
  of f: @Sendable @escaping @differentiable(reverse) (T) async -> R
) async -> (T) async -> R.TangentVector
  where T.TangentVector == T {
  return { x in await derivative(at: x, of: f) }
}

@inlinable
@available(StdlibDeploymentTarget 6.5, *)
public func derivative<T: FloatingPoint, U: FloatingPoint, R>(
  of f: @Sendable @escaping @differentiable(reverse) (T, U) async -> R
) async -> (T, U) async -> R.TangentVector
  where T.TangentVector == T,
        U.TangentVector == U {
  return { (x, y) in await derivative(at: x, y, of: f) }
}

@inlinable
@available(StdlibDeploymentTarget 6.5, *)
public func derivative<T: FloatingPoint, U: FloatingPoint, V: FloatingPoint, R>(
  of f: @Sendable @escaping @differentiable(reverse) (T, U, V) async -> R
) async -> (T, U, V) async -> R.TangentVector
  where T.TangentVector == T,
        U.TangentVector == U,
        V.TangentVector == V {
  return { (x, y, z) in await derivative(at: x, y, z, of: f) }
}

// Gradient (curried)

@inlinable
@available(StdlibDeploymentTarget 6.5, *)
public func gradient<T, R>(
  of f: @Sendable @escaping @differentiable(reverse) (T) async -> R
) async -> (T) async -> T.TangentVector
  where R : FloatingPoint, R.TangentVector == R {
  return { x in await gradient(at: x, of: f) }
}

@inlinable
@available(StdlibDeploymentTarget 6.5, *)
public func gradient<T, U, R>(
  of f: @Sendable @escaping @differentiable(reverse) (T, U) async -> R
) async -> (T, U) async -> (T.TangentVector, U.TangentVector)
  where R : FloatingPoint, R.TangentVector == R {
  return { x, y in await gradient(at: x, y, of: f) }
}

@inlinable
@available(StdlibDeploymentTarget 6.5, *)
public func gradient<T, U, V, R>(
  of f: @Sendable @escaping @differentiable(reverse) (T, U, V) async -> R
) async -> (T, U, V) async -> (T.TangentVector, U.TangentVector, V.TangentVector)
  where R : FloatingPoint, R.TangentVector == R {
  return { x, y, z in await gradient(at: x, y, z, of: f) }
}
