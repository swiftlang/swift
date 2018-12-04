//===-- CompositeMath.swift -----------------------------------*- swift -*-===//
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
// This file contains composite math functions. Functions in this file are
// defined in terms of core ops that are differentiable, and therefore do not
// need custom gradients.
//
//===----------------------------------------------------------------------===//

/// Computes `sigmoid` of the specified tensor element-wise.
/// Specifically, computes `1 / (1 + exp(-x))`.
@inlinable @inline(__always)
public func sigmoid<T : BinaryFloatingPoint>(_ x: Tensor<T>) -> Tensor<T> {
  return 1 / (1 + exp(-x))
}

/// Computes `relu` of the specified tensor element-wise.
/// Specifically, computes `max(0, x)`.
@inlinable @inline(__always)
@differentiable(reverse, adjoint: _adjointRelu(_:_:_:))
public func relu<T : BinaryFloatingPoint>(_ x: Tensor<T>) -> Tensor<T> {
  return max(0, x)
}

/// Computes the softmax of the specified tensor element-wise.
/// Specifically, computes `exp(x) / exp(x).sum()`.
@inlinable @inline(__always)
public func softmax<T : BinaryFloatingPoint>(_ x: Tensor<T>) -> Tensor<T> {
  let expx = exp(x)
  let sum = expx.sum()
  return expx / sum
}

/// Computes the softmax of the specified tensor along the specified axis.
/// Specifically, computes `exp(x) / exp(x).sum(alongAxes: axis)`.
@inlinable @inline(__always)
public func softmax<T : BinaryFloatingPoint>(
  _ x: Tensor<T>, alongAxis axis: Int32
) -> Tensor<T> {
  let expx = exp(x)
  return expx / expx.sum(alongAxes: axis)
}
