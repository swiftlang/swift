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
@_fixed_layout
public struct Dataset<InputScalar : AccelerableByTensorFlow,
                      OutputScalar : AccelerableByTensorFlow> {
  @_versioned let dataSource: String = "mnist"
  public let filePath: String
  public let batchSize: Int32
  public let inputShape: TensorShape
  public let outputShape: TensorShape

  @_inlineable @inline(__always)
  public init(filePath: String, batchSize: Int32,
              inputShape: TensorShape, outputShape: TensorShape) {
    self.filePath = filePath
    self.batchSize = batchSize
    self.inputShape = inputShape
    self.outputShape = outputShape
  }

  public typealias Element = (Tensor<InputScalar>, Tensor<OutputScalar>)

  @_inlineable @inline(__always)
  public mutating func next() -> Element {
    typealias ElementHandle = (TensorHandle<InputScalar>,
                               TensorHandle<OutputScalar>)
    let (xHandle, yHandle): ElementHandle = #tfop(
      "tfc.makeIteratorGetNextWithDatasets",
      dataSource: dataSource,
      filePath: filePath,
      batchSize: batchSize,
      output_shapes: [inputShape, outputShape]
    )
    return (#tfop("Identity", xHandle), #tfop("Identity", yHandle))
  }
}
