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
// This file defines the `ConvertibleFromNumpyArray` protocol for bridging
// `numpy.ndarray`.
//
//===----------------------------------------------------------------------===//

/// The `numpy` Python module.
/// Note: Global variables are lazy, so the following declaration won't produce
// a Python import error until it is first used.
private let np = Python.import("numpy")

/// A type that can be initialized from a `numpy.ndarray` instance represented
/// as a `PythonObject`.
public protocol ConvertibleFromNumpyArray {
  init?(numpyArray: PythonObject)
}

/// A type that is compatible with a NumPy scalar `dtype`.
public protocol NumpyScalarCompatible {
  static func isCompatible(withNumpyScalarType dtype: PythonObject) -> Bool
}

extension Bool : NumpyScalarCompatible {
  public static func isCompatible(
    withNumpyScalarType dtype: PythonObject
  ) -> Bool {
    switch dtype {
    case np.bool_, Python.bool: return true
    default: return false
    }
  }
}

extension UInt8 : NumpyScalarCompatible {
  public static func isCompatible(
    withNumpyScalarType dtype: PythonObject
  ) -> Bool {
    switch dtype {
    case np.uint8: return true
    default: return false
    }
  }
}

extension Int8 : NumpyScalarCompatible {
  public static func isCompatible(
    withNumpyScalarType dtype: PythonObject
  ) -> Bool {
    switch dtype {
    case np.int8: return true
    default: return false
    }
  }
}

extension UInt16 : NumpyScalarCompatible {
  public static func isCompatible(
    withNumpyScalarType dtype: PythonObject
  ) -> Bool {
    switch dtype {
    case np.uint16: return true
    default: return false
    }
  }
}

extension Int16 : NumpyScalarCompatible {
  public static func isCompatible(
    withNumpyScalarType dtype: PythonObject
  ) -> Bool {
    switch dtype {
    case np.int16: return true
    default: return false
    }
  }
}

extension UInt32 : NumpyScalarCompatible {
  public static func isCompatible(
    withNumpyScalarType dtype: PythonObject
  ) -> Bool {
    switch dtype {
    case np.uint32: return true
    default: return false
    }
  }
}

extension Int32 : NumpyScalarCompatible {
  public static func isCompatible(
    withNumpyScalarType dtype: PythonObject
  ) -> Bool {
    switch dtype {
    case np.int32: return true
    default: return false
    }
  }
}

extension UInt64 : NumpyScalarCompatible {
  public static func isCompatible(
    withNumpyScalarType dtype: PythonObject
  ) -> Bool {
    switch dtype {
    case np.uint64: return true
    default: return false
    }
  }
}

extension Int64 : NumpyScalarCompatible {
  public static func isCompatible(
    withNumpyScalarType dtype: PythonObject
  ) -> Bool {
    switch dtype {
    case np.int64: return true
    default: return false
    }
  }
}

extension Float : NumpyScalarCompatible {
  public static func isCompatible(
    withNumpyScalarType dtype: PythonObject
  ) -> Bool {
    switch dtype {
    case np.float32: return true
    default: return false
    }
  }
}

extension Double : NumpyScalarCompatible {
  public static func isCompatible(
    withNumpyScalarType dtype: PythonObject
  ) -> Bool {
    switch dtype {
    case np.float64: return true
    default: return false
    }
  }
}

/// A helper struct with all the data that ConvertibleFromNumpyArray
/// implementations need to copy memory from a NumpyArray.
public struct ContiguousNumpyArray<Scalar, ShapeSize> {
  /// The shape of the array.
  public let shape: [ShapeSize]

  /// A pointer to the memory, which is guaranteed to hold the array data in
  /// C_CONTIGUOUS format.
  public let ptr: UnsafePointer<Scalar>

  /// The array itself. We hold it in this struct so that `ptr` points to valid
  /// memory as long as this struct is around.
  private let contiguousNumpyArray: PythonObject
}

extension ContiguousNumpyArray : ConvertibleFromNumpyArray
  where Scalar : NumpyScalarCompatible, ShapeSize : PythonConvertible {
  public init?(numpyArray: PythonObject) {
    // Check if input is a `numpy.ndarray` instance.
    guard Python.isinstance(numpyArray, np.ndarray) == true else {
      return nil
    }
    // Check if the dtype of the `ndarray` is compatible with the `Scalar`
    // type.
    guard Scalar.isCompatible(withNumpyScalarType: numpyArray.dtype) else {
      return nil
    }

    let pyShape = numpyArray.__array_interface__["shape"]
    guard let shape = Array<ShapeSize>(pyShape) else {
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

    self.shape = shape
    self.ptr = ptr
    self.contiguousNumpyArray = contiguousNumpyArray
  }
}

extension Array : ConvertibleFromNumpyArray
  where Element : NumpyScalarCompatible {
  public init?(numpyArray: PythonObject) {
    guard let array = ContiguousNumpyArray<Element, Int>(numpyArray: numpyArray)
      else { return nil }

    // Only 1-D `ndarray` instances can be converted to `Array`.
    guard array.shape.count == 1 else {
      return nil
    }

    // This code avoids constructing and initialize from `UnsafeBufferPointer`
    // because that uses the `init<S : Sequence>(_ elements: S)` initializer,
    // which performs unnecessary copying.
    let dummyPointer = UnsafeMutablePointer<Element>.allocate(capacity: 1)
    let scalarCount = array.shape.reduce(1, *)
    self.init(repeating: dummyPointer.move(), count: scalarCount)
    dummyPointer.deallocate()
    withUnsafeMutableBufferPointer { buffPtr in
      buffPtr.baseAddress!.assign(from: array.ptr, count: scalarCount)
    }
  }
}
