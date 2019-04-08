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
// NOTE:
// - Currently, we do not want to expose VJP functions to users. The name of
//   each VJP function should start with an underscore.
//
// TODO:
// - Fix VJPs for broadcasting ops (need to perform reduction).
//
//===----------------------------------------------------------------------===//

infix operator .== : ComparisonPrecedence
infix operator .> : ComparisonPrecedence

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

//===----------------------------------------------------------------------===//
// Elementwise binary
//===----------------------------------------------------------------------===//

extension Tensor where Scalar : TensorFlowFloatingPoint {
  @inlinable
  static func _vjpAdd(
    lhs: Tensor, rhs: Tensor
  ) -> (Tensor, (Tensor) -> (Tensor, Tensor)) {
    return (lhs + rhs, {
      [lhsShape = lhs.shapeTensor, rhsShape = rhs.shapeTensor] v in
      return (v.unbroadcast(toShape: lhsShape), v.unbroadcast(toShape: rhsShape))
    })
  }

  @inlinable
  static func _vjpSubtract(
    lhs: Tensor, rhs: Tensor
  ) -> (Tensor, (Tensor) -> (Tensor, Tensor)) {
    return (lhs - rhs, {
      [lhsShape = lhs.shapeTensor, rhsShape = rhs.shapeTensor] v in
      return (v.unbroadcast(toShape: lhsShape),
              -v.unbroadcast(toShape: rhsShape))
    })
  }

  @inlinable
  static func _vjpMultiply(
    lhs: Tensor, rhs: Tensor
  ) -> (Tensor, (Tensor) -> (Tensor, Tensor)) {
    return (lhs * rhs, {
      [lhsShape = lhs.shapeTensor, rhsShape = rhs.shapeTensor] v in
      ((rhs * v).unbroadcast(toShape: lhsShape),
       (lhs * v).unbroadcast(toShape: rhsShape))
    })
  }

  @inlinable
  static func _vjpDivide(
    lhs: Tensor, rhs: Tensor
  ) -> (Tensor, (Tensor) -> (Tensor, Tensor)) {
    return (lhs / rhs, {
      [lhsShape = lhs.shapeTensor, rhsShape = rhs.shapeTensor] v in
      ((v / rhs).unbroadcast(toShape: lhsShape),
       ((-lhs) / rhs.squared() * v).unbroadcast(toShape: rhsShape))
    })
  }
}

extension Tensor where Scalar : TensorFlowFloatingPoint {
  @inlinable
  static func _vjpAdd(
    lhs: Tensor, rhs: Scalar
  ) -> (Tensor, (Tensor) -> (Tensor, Scalar)) {
    return (lhs + rhs, { v in (v, v.sum().scalarized()) })
  }

   @inlinable
  static func _vjpAdd(
    lhs: Scalar, rhs: Tensor
  ) -> (Tensor, (Tensor) -> (Scalar, Tensor)) {
    return (lhs + rhs, { v in (v.sum().scalarized(), v) })
  }

  @inlinable
  static func _vjpSubtract(
    lhs: Tensor, rhs: Scalar
  ) -> (Tensor, (Tensor) -> (Tensor, Scalar)) {
    return (lhs - rhs, { v in (v, 0 - v.sum().scalarized()) })
  }

  @inlinable
  static func _vjpSubtract(
    lhs: Scalar, rhs: Tensor
  ) -> (Tensor, (Tensor) -> (Scalar, Tensor)) {
    return (lhs - rhs, { v in (v.sum().scalarized(), 0 - v) })
  }

  @inlinable
  static func _vjpMultiply(
    lhs: Tensor, rhs: Scalar
  ) -> (Tensor, (Tensor) -> (Tensor, Scalar)) {
    return (lhs * rhs, { v in (v * rhs, (v * lhs).sum().scalarized()) })
  }

  @inlinable
  static func _vjpMultiply(
    lhs: Scalar, rhs: Tensor
  ) -> (Tensor, (Tensor) -> (Scalar, Tensor)) {
    return (lhs * rhs, { v in ((v * rhs).sum().scalarized(), v * lhs) })
  }

  @inlinable
  static func _vjpDivide(
    lhs: Tensor, rhs: Scalar
  ) -> (Tensor, (Tensor) -> (Tensor, Scalar)) {
    return (lhs / rhs, { v in
      (v / rhs, (v * (0 - lhs) / Tensor(rhs).squared()).sum().scalarized())
    })
  }

  @inlinable
  static func _vjpDivide(
    lhs: Scalar, rhs: Tensor
  ) -> (Tensor, (Tensor) -> (Scalar, Tensor)) {
    return (lhs / rhs, { v in
      ((v / rhs).sum().scalarized(), v * -lhs / rhs.squared())
    })
  }
}

@inlinable
func _vjpMinMaxHelper<T : TensorFlowFloatingPoint>(
  _ x: Tensor<T>, _ y: Tensor<T>, originalValue: Tensor<T>, vector: Tensor<T>
) -> (Tensor<T>, Tensor<T>) {
  let denom = 1 + Tensor<T>(x .== y)
  let dfdx = vector * Tensor<T>(x .== originalValue) / denom
  let dfdy = vector * Tensor<T>(y .== originalValue) / denom
  return (dfdx.unbroadcast(like: x), dfdy.unbroadcast(like: y))
}

@inlinable
func _vjpMax<T : TensorFlowFloatingPoint>(
  _ x: Tensor<T>, _ y: Tensor<T>
) -> (Tensor<T>, (Tensor<T>) -> (Tensor<T>, Tensor<T>)) {
  let value = max(x, y)
  return (value,
    { v in _vjpMinMaxHelper(x, y, originalValue: value, vector: v) })
}

@inlinable
func _vjpMin<T : TensorFlowFloatingPoint>(
  _ x: Tensor<T>, _ y: Tensor<T>
) -> (Tensor<T>, (Tensor<T>) -> (Tensor<T>, Tensor<T>)) {
  let value = min(x, y)
  return (value,
    { v in _vjpMinMaxHelper(x, y, originalValue: value, vector: v) })
}

@inlinable
func _vjpPow<T : TensorFlowFloatingPoint>(
  _ x: Tensor<T>, _ y: Tensor<T>
) -> (Tensor<T>, (Tensor<T>) -> (Tensor<T>, Tensor<T>)) {
  let value = pow(x, y)
  return (value, { v in
    ((v * y * pow(x, y-1)).unbroadcast(like: x),
     (v * log(x) * value).unbroadcast(like: y))
  })
}

//===----------------------------------------------------------------------===//
// Elementwise unary
//===----------------------------------------------------------------------===//

extension Tensor where Scalar : TensorFlowFloatingPoint {
  @inlinable
  static func _vjpNegate(_ x: Tensor) -> (Tensor, (Tensor) -> Tensor) {
    return (-x, { v in -v })
  }
}

@inlinable
func _vjpAbs<T : TensorFlowFloatingPoint>(
  _ x: Tensor<T>
) -> (Tensor<T>, (Tensor<T>) -> Tensor<T>) {
  let sign = Raw.sign(x)
  return (abs(x), { v in v * sign })
}

@inlinable
func _vjpLog<T : TensorFlowFloatingPoint>(
  _ x: Tensor<T>
) -> (Tensor<T>, (Tensor<T>) -> Tensor<T>) {
  return (log(x), { v in v / x })
}

@inlinable
func _vjpSin<T : TensorFlowFloatingPoint>(
  _ x: Tensor<T>
) -> (Tensor<T>, (Tensor<T>) -> Tensor<T>) {
  return (sin(x), { v in v * cos(x) })
}

@inlinable
func _vjpCos<T : TensorFlowFloatingPoint>(
  _ x: Tensor<T>
) -> (Tensor<T>, (Tensor<T>) -> Tensor<T>) {
  return (cos(x), { v in -v * sin(x) })
}

@inlinable
func _vjpTan<T : TensorFlowFloatingPoint>(
  _ x: Tensor<T>
) -> (Tensor<T>, (Tensor<T>) -> Tensor<T>) {
  let value = tan(x)
  return (value, { v in v * (1 + value.squared()) })
}

@inlinable
func _vjpSinh<T : TensorFlowFloatingPoint>(
  _ x: Tensor<T>
) -> (Tensor<T>, (Tensor<T>) -> Tensor<T>) {
  return (sinh(x), { v in v * cosh(x) })
}

@inlinable
func _vjpCosh<T : TensorFlowFloatingPoint>(
  _ x: Tensor<T>
) -> (Tensor<T>, (Tensor<T>) -> Tensor<T>) {
  return (cosh(x), { v in v * sinh(x) })
}

@inlinable
func _vjpTanh<T : TensorFlowFloatingPoint>(
  _ x: Tensor<T>
) -> (Tensor<T>, (Tensor<T>) -> Tensor<T>) {
  let value = tanh(x)
  return (value, { v in v * (1 - value.squared()) })
}

@inlinable
func _vjpExp<T : TensorFlowFloatingPoint>(
  _ x: Tensor<T>
) -> (Tensor<T>, (Tensor<T>) -> Tensor<T>) {
  let value = exp(x)
  return (value, { v in value * v })
}

@inlinable
func _vjpCeil<T : TensorFlowFloatingPoint>(
  _ x: Tensor<T>
) -> (Tensor<T>, (Tensor<T>) -> Tensor<T>) {
  return (ceil(x), { _ in Tensor(0).broadcast(like: x) })
}

@inlinable
func _vjpFloor<T : TensorFlowFloatingPoint>(
  _ x: Tensor<T>
) -> (Tensor<T>, (Tensor<T>) -> Tensor<T>) {
  return (floor(x), { _ in Tensor(0).broadcast(like: x) })
}

@inlinable
func _vjpSqrt<T : TensorFlowFloatingPoint>(
  _ x: Tensor<T>
) -> (Tensor<T>, (Tensor<T>) -> Tensor<T>) {
  let value = sqrt(x)
  return (value, { v in v / (2 * value) })
}

@inlinable
func _vjpRsqrt<T : TensorFlowFloatingPoint>(
  _ x: Tensor<T>
) -> (Tensor<T>, (Tensor<T>) -> Tensor<T>) {
  let value = rsqrt(x)
  return (value, { v in -v / 2 * value })
}

@inlinable
func _vjpLogSoftmax<T : TensorFlowFloatingPoint>(
  _ x: Tensor<T>
) -> (Tensor<T>, (Tensor<T>) -> Tensor<T>) {
  let value = logSoftmax(x)
  return (value, { v in
    v - v.sum(alongAxes: -1) * exp(value)
  })
}

extension Tensor where Scalar : TensorFlowFloatingPoint {
  @inlinable
  func _vjpSquared() -> (Tensor, (Tensor) -> Tensor) {
    return (squared(), { 2 * self * $0 })
  }
}

//===----------------------------------------------------------------------===//
// Linear algebra
//===----------------------------------------------------------------------===//

@inlinable
func _vjpMatmul<Scalar : TensorFlowFloatingPoint>(
  _ lhs: Tensor<Scalar>, _ rhs: Tensor<Scalar>
) -> (Tensor<Scalar>, (Tensor<Scalar>) -> (Tensor<Scalar>, Tensor<Scalar>)) {
  let value = matmul(lhs, rhs)
  return (value, { v in 
    return (matmul(v, rhs.transposed()), matmul(lhs.transposed(), v))
  })
}

// TODO: We have to define a custom VJP on • because AD can't yet
// differentiate generic methods. After AD can differentiate generic methods,
// remove the custom VJP.
extension Tensor where Scalar : TensorFlowFloatingPoint {
  @inlinable
  static func _vjpMatmulOperator(
    lhs: Tensor, rhs: Tensor
  ) -> (Tensor, (Tensor) -> (Tensor, Tensor)) {
    return _vjpMatmul(lhs, rhs)
  }

  @inlinable
  func _vjpTransposed(
    withPermutations permutations: Tensor<Int32>
  ) -> (Tensor, (Tensor) -> Tensor) {
    let value = transposed(withPermutations: permutations)
    return (value, { $0.transposed(withPermutations: permutations) })
  }

  @inlinable
  func _vjpTransposed(
    withPermutations permutations: [Int32]
  ) -> (Tensor, (Tensor) -> Tensor) {
    let value = transposed(withPermutations: permutations)
    return (value, { $0.transposed(withPermutations: permutations) })
  }

  @inlinable
  func _vjpTransposed(
    withPermutations permutations: Int32...
  ) -> (Tensor, (Tensor) -> Tensor) {
    let value = transposed(withPermutations: permutations)
    return (value, { $0.transposed(withPermutations: permutations) })
  }

  @inlinable
  func _vjpTransposed() -> (Tensor, (Tensor) -> Tensor) {
    return (transposed(), { $0.transposed() })
  }
}

//===----------------------------------------------------------------------===//
// Shape transformations
//===----------------------------------------------------------------------===//

extension Tensor where Scalar : TensorFlowFloatingPoint {
  @inlinable
  func _vjpReshaped(
    toShape newShape: Tensor<Int32>
  ) -> (Tensor, (Tensor) -> Tensor) {
    let value = reshaped(toShape: newShape)
    return (value, { [shape = shapeTensor] v in
      v.reshaped(toShape: shape)
    })
  }

  @inlinable
  func _vjpSqueezingShape(at axes: [Int32]) -> (Tensor, (Tensor) -> Tensor) {
    let value = squeezingShape(at: axes)
    return (value, { [shape = shapeTensor] v in
      v.reshaped(toShape: shape)
    })
  }

  @inlinable
  func _vjpExpandingShape(
    at shapeIndex: Int32
  ) -> (Tensor, (Tensor) -> Tensor) {
    let value = expandingShape(at: shapeIndex)
    return (value, { v in
      v.squeezingShape(at: shapeIndex)
    })
  }
}

//===----------------------------------------------------------------------===//
// Reduction
//===----------------------------------------------------------------------===//

extension Tensor where Scalar : TensorFlowFloatingPoint {
  @inlinable
  func _vjpMean() -> (Tensor, (Tensor) -> Tensor) {
    return (mean(), { [shape = shapeTensor, count = scalarCountTensor] in
      ($0 / Tensor(count)).broadcast(toShape: shape)
    })
  }

  @inlinable
  func _vjpSum() -> (Tensor, (Tensor) -> Tensor) {
    return (sum(), { [shape = shapeTensor] in $0.broadcast(toShape: shape) })
  }

  @inlinable
  func _vjpMean(alongAxes axes: Tensor<Int32>) -> (Tensor, (Tensor) -> Tensor) {
    let value = mean(alongAxes: axes)
    let count = Raw.gather(params: shapeTensor, indices: axes).product()
    return (value, { [shape = shapeTensor] in
      $0.broadcast(toShape: shape) / Tensor(count)
    })
  }

  @inlinable
  func _vjpSum(alongAxes axes: [Int32]) -> (Tensor, (Tensor) -> Tensor) {
    let value = sum(alongAxes: axes)
    return (value, { [shape = shapeTensor] in $0.broadcast(toShape: shape) })
  }
}

//===----------------------------------------------------------------------===//
// Composite math
//===----------------------------------------------------------------------===//

@inlinable
func _vjpSigmoid<T : TensorFlowFloatingPoint>(
  _ x: Tensor<T>
) -> (Tensor<T>, (Tensor<T>) -> Tensor<T>) {
  let value = sigmoid(x)
  return (value, { v in Raw.sigmoidGrad(value, dy: v) })
}

@inlinable
func _vjpSoftmax<T : TensorFlowFloatingPoint>(
  _ x: Tensor<T>
) -> (Tensor<T>, (Tensor<T>) -> Tensor<T>) {
  let value = softmax(x)
  return (value, { v in
    let sumChannels = (v * value).sum(alongAxes: -1)
    return (v - sumChannels) * value
  })
}

@inlinable
func _vjpRelu<T : TensorFlowFloatingPoint>(
  _ x: Tensor<T>
) -> (Tensor<T>, (Tensor<T>) -> Tensor<T>) {
  return (relu(x), { v in Tensor(x .> 0) * v })
}
