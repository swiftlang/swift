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
public func sigmoid<Scalar : BinaryFloatingPoint, T : TensorProtocol>(
  _ x: T
) -> T where T.Scalar == Scalar {
  let expx = exp(-x)
  let one = T(handle: _TFMakeScalarTensor(1.0))
  return one / (one + expx)
  // NOTE: Scalar-tensor op implementation below. It cannot be used currently
  // because scalar-tensor binary ops are not defind on TensorProtocol.
  // let expx = exp(-x)
  // return 1.0 / (1.0 + expx)
}

@_inlineable @inline(__always)
public func relu<Scalar : Numeric & Comparable, T : TensorProtocol>(_ x: T) -> T
  where T.Scalar == Scalar {
  return max(0, x)
}

@_inlineable @inline(__always)
public func softmax<Scalar : FloatingPoint, T : TensorProtocol>(_ x: T) -> T
  where T.Scalar == Scalar {
  let expx = exp(x)
  let sum = T(handle: _TFMakeScalarTensor(expx.sum()))
  return expx / sum
  // NOTE: Scalar-tensor op implementation below. It cannot be used currently
  // because scalar-tensor binary ops are not defind on TensorProtocol.
  // let expx = exp(x)
  // return expx / expx.sum()
}
