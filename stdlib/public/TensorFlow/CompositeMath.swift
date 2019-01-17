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
@differentiable(vjp: _vjpSigmoid(_:) where T : Differentiable)
public func sigmoid<T : FloatingPoint>(_ x: Tensor<T>) -> Tensor<T> {
  return Raw.sigmoid(x)
}

/// Computes `relu` of the specified tensor element-wise.
/// Specifically, computes `max(0, x)`.
@inlinable @inline(__always)
@differentiable(vjp: _vjpRelu(_:) where T : Differentiable)
public func relu<T : FloatingPoint>(_ x: Tensor<T>) -> Tensor<T> {
  return max(0, x)
}

/// Computes the softmax of the specified tensor along the last axis.
/// Specifically, computes `exp(x) / exp(x).sum(alongAxes: -1)`.
@inlinable @inline(__always)
@differentiable(vjp: _vjpSoftmax(_:) where T : Differentiable)
public func softmax<T : FloatingPoint>(_ x: Tensor<T>) -> Tensor<T> {
  return Raw.softmax(logits: x)
}

/// Computes the softmax of the specified tensor along the specified axis.
/// Specifically, computes `exp(x) / exp(x).sum(alongAxes: axis)`.
@inlinable @inline(__always)
public func softmax<T : Differentiable & FloatingPoint>(
  _ x: Tensor<T>, alongAxis axis: Int32
) -> Tensor<T> {
  let expx = exp(x)
  return expx / expx.sum(alongAxes: axis)
}
