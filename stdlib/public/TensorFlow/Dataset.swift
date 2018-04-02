//===-- Dataset.swift -----------------------------------------*- swift -*-===//
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
// This file contains the dataset API.
//
//===----------------------------------------------------------------------===//

// FIXME: This is just a nice-looking API wrapping the infeed
// `makeIteratorGetNextWithDatasets` intrinsics that are actually ad-hoc. When
// the compiler supports infeed in a more generic way, we will redesign this.
// This API should not be used for anything other than the Swift for TensorFlow
// demo.
@_versioned
@_fixed_layout
struct Dataset<InputScalar : AccelerableByTensorFlow,
               OutputScalar : AccelerableByTensorFlow> {
  public typealias Element = (Tensor<InputScalar>, Tensor<OutputScalar>)

  @_inlineable @inline(__always)
  public init(filePath: String, batchSize: Int32,
              inputShape: TensorShape, outputShape: TensorShape) {
    fatalError("Unimplemented")
  }
}
