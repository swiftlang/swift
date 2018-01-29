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

@_inlineable
public func sigmoid<Unit : FloatingPoint>(_ x: Tensor<Unit>) -> Tensor<Unit> {
  let expx = exp(-x)
  return 1 / (1 + expx)
}

@_inlineable
public func relu<Unit : Numeric & Comparable>(
  _ x: Tensor<Unit>
) -> Tensor<Unit> {
  return max(0, x)
}

@_inlineable
public func softmax<Unit : FloatingPoint>(_ x: Tensor<Unit>) -> Tensor<Unit> {
  let expx = exp(x)
  return expx / expx.sum()
}
