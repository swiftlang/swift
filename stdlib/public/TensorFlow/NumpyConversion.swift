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
    // Check if input is a `numpy.ndarray` instance.
    guard Python.isinstance(numpyArray, np.ndarray) == true else {
      return nil
    }
    // Check if the dtype of the `ndarray` is compatible with the `Scalar`
    // type.
    guard Scalar.numpyScalarTypes.contains(numpyArray.dtype) else {
      return nil
    }

    let pyShape = numpyArray.__array_interface__["shape"]
    guard let shape = Array<Int>(pyShape) else {
      return nil
    }

    // Make sure that the array is contiguous in memory. This does a copy if
    // the array is not already contiguous in memory.
    let contiguousNumpyArray = np.ascontiguousarray(numpyArray)

    guard let ptrVal =
      UInt(contiguousNumpyArray.__array_interface__["data"].tuple2.0) else {
      return nil
    }
    // Note: `ptr` is not nil even if the `ndarray` is empty (i.e. has a shape
    // of `(0,)`).
    guard let ptr = UnsafePointer<Scalar>(bitPattern: ptrVal) else {
      fatalError("numpy.ndarray data pointer was nil")
    }
    // This code avoids calling `init<S : Sequence>(shape: [Int], scalars: S)`,
    // which inefficiently copies scalars one by one. Instead,
    // `init(shape: [Int], scalars: [Scalar])` is called, which efficiently
    // does a `memcpy` of the entire `scalars` array.
    // Unecessary copying is minimized.
    let dummyPointer = UnsafeMutablePointer<Scalar>.allocate(capacity: 1)
    let scalarCount = shape.reduce(1, *)
    var scalars: [Scalar] = Array(repeating: dummyPointer.move(),
                                  count: scalarCount)
    dummyPointer.deallocate()
    scalars.withUnsafeMutableBufferPointer { buffPtr in
      buffPtr.baseAddress!.assign(from: ptr, count: scalarCount)
    }
    self.init(shape: shape, scalars: scalars)
  }
}

extension Tensor : ConvertibleFromNumpyArray
  where Scalar : NumpyScalarCompatible {
  public init?(numpyArray: PythonObject) {
    // Check if input is a `numpy.ndarray` instance.
    guard Python.isinstance(numpyArray, np.ndarray) == true else {
      return nil
    }
    // Check if the dtype of the `ndarray` is compatible with the `Scalar`
    // type.
    guard Scalar.numpyScalarTypes.contains(numpyArray.dtype) else {
      return nil
    }

    let pyShape = numpyArray.__array_interface__["shape"]
    guard let dimensions = Array<Int32>(pyShape) else {
      return nil
    }
    let shape = TensorShape(dimensions)

    // Make sure that the array is contiguous in memory. This does a copy if
    // the array is not already contiguous in memory.
    let contiguousNumpyArray = np.ascontiguousarray(numpyArray)

    guard let ptrVal =
      UInt(contiguousNumpyArray.__array_interface__["data"].tuple2.0) else {
      return nil
    }
    // Note: `ptr` is not nil even if the `ndarray` is empty (i.e. has a shape
    // of `(0,)`).
    guard let ptr = UnsafePointer<Scalar>(bitPattern: ptrVal) else {
      fatalError("numpy.ndarray data pointer was nil")
    }
    let buffPtr = UnsafeBufferPointer(start: ptr,
                                      count: Int(shape.contiguousSize))
    self.init(shape: shape, scalars: buffPtr)
  }
}

extension ShapedArray where Scalar : NumpyScalarCompatible {
  /// Creates a NumPy array with the same elements.
  ///
  /// - Precondition: The `numpy` Python package must have been installed.
  public func makeNumpyArray() -> PythonObject {
    return scalars.makeNumpyArray().reshape(shape)
  }
}

extension Tensor where Scalar : NumpyScalarCompatible {
  public func makeNumpyArray() -> PythonObject { return array.makeNumpyArray() }
}

#endif
