//===-- DatasetPythonInitializers.swift -----------------------*- swift -*-===//
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
// This file defines Dataset initializers that use Python libraries to load
// the data.
//
// Using Python libraries is a temporary internal implementation detail. We
// intend to reimplement these initializers using TensorFlow ops (e.g.
// TextLineDataset and DecodeCSV) once that becomes possible. It's currently
// blocked on SR-8573 and SR-8574.
//
//===----------------------------------------------------------------------===//

#if canImport(Python)
import Python

extension SingleValueDataset where ScalarOfElement : NumpyScalarCompatible {
  @inlinable @inline(__always)
  public init(csvFilename: String, header: Bool, selectColumns: [Int]) {
    // We can't make `np` a private top-level variable in this file, because
    // this function is @inlinable.
    let np = Python.import("numpy")

    let numpyArray = np.loadtxt(csvFilename, delimiter: ",",
                                skiprows: header ? 1 : 0,
                                usecols: selectColumns,
                                dtype: ScalarOfElement.numpyScalarType)
    guard let tensor = Tensor<ScalarOfElement>(numpyArray: numpyArray) else {
      // This should never happen, because we construct numpyArray in such a
      // way that it should be convertible to tensor.
      fatalError("np.loadtxt result can't be converted to Tensor")
    }
    self.init(elements: tensor)
  }
}

extension DoubleValueDataset where ScalarOfFirstElement : NumpyScalarCompatible {
  @inlinable @inline(__always)
  public init(csvFilename: String, header: Bool,
              selectColumns: ([Int], [Int])) {
    // Reading the csv twice is, of course, absurdly inefficient. I didn't
    // write something more efficient because we're going to throw away this
    // implementation soon.
    let first = SingleValueDataset<ScalarOfFirstElement>(
        csvFilename: csvFilename, header: header,
        selectColumns: selectColumns.0)
    let second = SingleValueDataset<ScalarOfFirstElement>(
        csvFilename: csvFilename, header: header,
        selectColumns: selectColumns.1)
    self = zip(first, second)
  }
}

#endif
