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

@_inlineable @inline(__always)
public func sigmoid<T : TensorProtocol>(_ x: T) -> T
  where T.Scalar : BinaryFloatingPoint {
  let expx = exp(-x)
  return 1.0 / (1.0 + expx)
}

@_inlineable @inline(__always)
@differentiable(gradient: _adjointRelu(_:partial:seed:))
public func relu<T : TensorProtocol>(_ x: T) -> T
  where T.Scalar : FloatingPoint {
  return max(0, x)
}

@_inlineable @inline(__always)
public func softmax<T : TensorProtocol>(_ x: T) -> T
  where T.Scalar : FloatingPoint {
  let expx = exp(x)
  return expx / expx.sum()
}

@_inlineable @inline(__always)
public func softmax<T : TensorProtocol>(
  _ x: T, alongAxis axis: Int32
) -> T
  where T.Scalar : FloatingPoint {
  let expx = exp(x)
  return expx / expx.sum(alongAxes: axis)
}
