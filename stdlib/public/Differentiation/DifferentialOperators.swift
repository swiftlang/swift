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

// Value with differential

@inlinable
public func valueWithDifferential<each T, R>(
  at x: repeat each T, of f: @differentiable(reverse) (repeat each T) -> R
) -> (value: R, differential: (repeat (each T).TangentVector) -> R.TangentVector) {
  return Builtin.applyDerivative_jvp(f, repeat each x)
}

// Value with pullback
@inlinable
public func valueWithPullback<each T, R>(
  at x: repeat each T, of f: @differentiable(reverse) (repeat each T) -> R
) -> (value: R, pullback: (R.TangentVector) -> (repeat (each T).TangentVector)) {
  return Builtin.applyDerivative_vjp(f, repeat each x)
}

// Differential

@inlinable
public func differential<each T, R>(
  at x: repeat each T, of f: @differentiable(reverse) (repeat each T) -> R
) -> (repeat (each T).TangentVector) -> R.TangentVector {
  return valueWithDifferential(at: repeat each x, of: f).1
}

// Pullback

@inlinable
public func pullback<each T, R>(
  at x: repeat each T, of f: @differentiable(reverse) (repeat each T) -> R
) -> (R.TangentVector) -> (repeat (each T).TangentVector) {
  return valueWithPullback(at: repeat each x, of: f).1
}

// Derivative

@inlinable
public func derivative<each T: FloatingPoint, R>(
  at x: repeat each T, of f: @differentiable(reverse) (repeat each T) -> R
) ->  R.TangentVector
  where repeat (each T).TangentVector == each T {
  return differential(at: repeat each x, of: f)(repeat (each T).init(1))
}

// Gradient

@inlinable
public func gradient<each T, R>(
  at x: repeat each T, of f: @differentiable(reverse) (repeat each T) -> R
) -> (repeat (each T).TangentVector)
  where R : FloatingPoint, R.TangentVector == R {
  return pullback(at: repeat each x, of: f)(R(1))
}

// Value with derivative

@inlinable
public func valueWithDerivative<each T: FloatingPoint, R>(
  at x: repeat each T, of f: @escaping @differentiable(reverse) (repeat each T) -> R
) -> (value: R, derivative: R.TangentVector)
  where repeat (each T).TangentVector == each T {
  let (y, differential) = valueWithDifferential(at: repeat each x, of: f)
  return (y, differential(repeat (each T).init(1)))
}

// Value with gradient

@inlinable
public func valueWithGradient<each T, R>(
  at x: repeat each T, of f: @differentiable(reverse) (repeat each T) -> R
) -> (value: R, gradient: (repeat (each T).TangentVector))
  where R : FloatingPoint, R.TangentVector == R {
  let (y, pullback) = valueWithPullback(at: repeat each x, of: f)
  return (y, pullback(R(1)))
}

// Derivative (curried)

@inlinable 
public func derivative<each T: FloatingPoint, R>(
  of f: @escaping @differentiable(reverse) (repeat each T) -> R
) -> (repeat each T) -> R.TangentVector
  where repeat (each T).TangentVector == each T {
  return { (x: repeat each T) in derivative(at: repeat each x, of: f) }
}

// Gradient (curried)

@inlinable
public func gradient<each T, R>(
  of f: @escaping @differentiable(reverse) (repeat each T) -> R
) -> (repeat each T) -> (repeat (each T).TangentVector)
  where R : FloatingPoint, R.TangentVector == R {
  return { (x: repeat each T) in gradient(at: repeat each x, of: f) }
}
