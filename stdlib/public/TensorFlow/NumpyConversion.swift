//===-- NumpyConversion.swift ---------------------------------*- swift -*-===//
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
// This file defines conversion initializers from `numpy.ndarray` to
// `ShapedArray` and `Tensor`.
//
//===----------------------------------------------------------------------===//

#if canImport(Python)
import Python

/// The `numpy` Python module.
/// Note: Global variables are lazy, so the following declaration won't produce
// a Python import error until it is first used.
private let np = Python.import("numpy")

extension ShapedArray : ConvertibleFromNumpyArray
  where Scalar : NumpyScalarCompatible {
  public init?(numpyArray: PythonObject) {
    guard let array = ContiguousNumpyArray<Scalar, Int>(numpyArray: numpyArray)
      else { return nil }

    // This code avoids calling `init<S : Sequence>(shape: [Int], scalars: S)`,
    // which inefficiently copies scalars one by one. Instead,
    // `init(shape: [Int], scalars: [Scalar])` is called, which efficiently
    // does a `memcpy` of the entire `scalars` array.
    // Unecessary copying is minimized.
    let dummyPointer = UnsafeMutablePointer<Scalar>.allocate(capacity: 1)
    let scalarCount = array.shape.reduce(1, *)
    var scalars: [Scalar] = Array(repeating: dummyPointer.move(),
                                  count: scalarCount)
    dummyPointer.deallocate()
    scalars.withUnsafeMutableBufferPointer { buffPtr in
      buffPtr.baseAddress!.assign(from: array.ptr, count: scalarCount)
    }
    self.init(shape: array.shape, scalars: scalars)
  }
}

extension Tensor : ConvertibleFromNumpyArray
  where Scalar : NumpyScalarCompatible {
  public init?(numpyArray: PythonObject) {
    guard let array = ContiguousNumpyArray<Scalar, Int32>(
        numpyArray: numpyArray) else {
      return nil
    }

    let tensorShape = TensorShape(array.shape)
    let buffPtr = UnsafeBufferPointer(start: array.ptr,
                                      count: Int(tensorShape.contiguousSize))
    self.init(shape: tensorShape, scalars: buffPtr)
  }
}
#endif
