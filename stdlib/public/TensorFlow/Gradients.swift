//===-- Gradients.swift ---------------------------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file contains vector-Jacobian product (VJP) definitions for Tensor ops.
//
// Terminology:
// - originalValue (f): The function being differentiated, or the result of that
//   function.
// - VJP (f'): The function as the result of differentiation, computing
//   the vector-Jacobian products with respect to all arguments, or the result
//   of that function.
//
// For more information, visit:
// https://en.wikipedia.org/wiki/Automatic_differentiation
//
// Every function in this file is the VJP of some corresponding function
// defined in Ops.swift, with respect to all arguments. The attribute
// '@differentiable(vjp: ...)' is used to register a function's VJP. The
// automatic differentiation pass identifies these VJPs and chains them
// together to produce arbitrary differentiable programs.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Method-style differential operators
//===----------------------------------------------------------------------===//

public extension Differentiable {
  @inlinable
  func gradient<R : TensorFlowFloatingPoint>(
    in f: @differentiable (Self) -> Tensor<R>
  ) -> CotangentVector {
    return self.pullback(in: f)(Tensor<R>(1))
  }

  @inlinable
  func valueWithGradient<R : TensorFlowFloatingPoint>(
    in f: @differentiable (Self) -> Tensor<R>
  ) -> (value: Tensor<R>, gradient: CotangentVector) {
    let (y, pb) = self.valueWithPullback(in: f)
    return (y, pb(Tensor<R>(1)))
  }

  @inlinable
  func gradient<T : Differentiable, R : TensorFlowFloatingPoint>(
    at x: T, in f: @differentiable (Self, T) -> Tensor<R>
  ) -> (CotangentVector, T.CotangentVector) {
    return self.pullback(at: x, in: f)(Tensor<R>(1))
  }

  @inlinable
  func valueWithGradient<T : Differentiable, R : TensorFlowFloatingPoint>(
    at x: T, in f: @differentiable (Self, T) -> Tensor<R>
  ) -> (value: Tensor<R>, gradient: (CotangentVector, T.CotangentVector)) {
    let (y, pb) = self.valueWithPullback(at: x, in: f)
    return (y, pb(Tensor<R>(1)))
  }
}

//===----------------------------------------------------------------------===//
// Free-function-style differential operators
//===----------------------------------------------------------------------===//

// Value with gradient

@inlinable
public func valueWithGradient<T, R>(
  at x: T, in f: @differentiable (T) -> Tensor<R>
) -> (value: Tensor<R>, gradient: T.CotangentVector)
where T : Differentiable, R : TensorFlowFloatingPoint {
  let (y, pullback) = valueWithPullback(at: x, in: f)
  return (y, pullback(Tensor<R>(1)))
}

@inlinable
public func valueWithGradient<T, U, R>(
  at x: T, _ y: U, in f: @differentiable (T, U) -> Tensor<R>
) -> (value: Tensor<R>, gradient: (T.CotangentVector, U.CotangentVector))
  where T : Differentiable, U : Differentiable,
        R : TensorFlowFloatingPoint {
  let (y, pullback) = valueWithPullback(at: x, y, in: f)
  return (y, pullback(Tensor<R>(1)))
}

@inlinable
public func valueWithGradient<T, U, V, R>(
  at x: T, _ y: U, _ z: V, in f: @differentiable (T, U, V) -> Tensor<R>
) -> (value: Tensor<R>,
      gradient: (T.CotangentVector, U.CotangentVector, V.CotangentVector))
  where T : Differentiable, U : Differentiable, V : Differentiable,
        R : TensorFlowFloatingPoint {
  let (y, pullback) = valueWithPullback(at: x, y, z, in: f)
  return (y, pullback(Tensor<R>(1)))
}

// Value with gradient (curried)

@inlinable
public func valueWithGradient<T, R>(
  of f: @escaping @differentiable (T) -> Tensor<R>
) -> (T) -> (value: Tensor<R>, gradient: T.CotangentVector)
  where T : Differentiable, R : TensorFlowFloatingPoint {
  return { x in valueWithGradient(at: x, in: f) }
}

@inlinable
public func valueWithGradient<T, U, R>(
  of f: @escaping @differentiable (T, U) -> Tensor<R>
) -> (T, U)
    -> (value: Tensor<R>, gradient: (T.CotangentVector, U.CotangentVector))
  where T : Differentiable, U : Differentiable,
        R : TensorFlowFloatingPoint {
  return { x, y in valueWithGradient(at: x, y, in: f) }
}

@inlinable
public func valueWithGradient<T, U, V, R>(
  of f: @escaping @differentiable (T, U, V) -> Tensor<R>
) -> (T, U, V)
    -> (value: Tensor<R>,
        gradient: (T.CotangentVector, U.CotangentVector, V.CotangentVector))
  where T : Differentiable, U : Differentiable, V : Differentiable,
        R : TensorFlowFloatingPoint {
  return { x, y, z in valueWithGradient(at: x, y, z, in: f) }
}

// Gradient

@inlinable
public func gradient<T, R>(
  at x: T, in f: @differentiable (T) -> Tensor<R>
) -> T.CotangentVector
  where T : Differentiable, R : TensorFlowFloatingPoint {
  return pullback(at: x, in: f)(Tensor<R>(1))
}

@inlinable
public func gradient<T, U, R>(
  at x: T, _ y: U, in f: @differentiable (T, U) -> Tensor<R>
) -> (T.CotangentVector, U.CotangentVector)
  where T : Differentiable, U : Differentiable,
        R : TensorFlowFloatingPoint {
  return pullback(at: x, y, in: f)(Tensor<R>(1))
}

@inlinable
public func gradient<T, U, V, R>(
  at x: T, _ y: U, _ z: V, in f: @differentiable (T, U, V) -> Tensor<R>
) -> (T.CotangentVector, U.CotangentVector, V.CotangentVector)
  where T : Differentiable, U : Differentiable, V : Differentiable,
        R : TensorFlowFloatingPoint {
  return pullback(at: x, y, z, in: f)(Tensor<R>(1))
}

// Gradient (curried)

@inlinable
public func gradient<T, R>(
  of f: @escaping @differentiable (T) -> Tensor<R>
) -> (T) -> T.CotangentVector
  where T : Differentiable, R : TensorFlowFloatingPoint {
  return { x in gradient(at: x, in: f) }
}

@inlinable
public func gradient<T, U, R>(
  of f: @escaping @differentiable (T, U) -> Tensor<R>
) -> (T, U) -> (T.CotangentVector, U.CotangentVector)
  where T : Differentiable, U : Differentiable,
        R : TensorFlowFloatingPoint {
  return { x, y in gradient(at: x, y, in: f) }
}

@inlinable
public func gradient<T, U, V, R>(
  of f: @escaping @differentiable (T, U, V) -> Tensor<R>
) -> (T, U, V) -> (T.CotangentVector, U.CotangentVector, V.CotangentVector)
  where T : Differentiable, U : Differentiable, V : Differentiable,
        R : TensorFlowFloatingPoint {
  return { x, y, z in gradient(at: x, y, z, in: f) }
}
