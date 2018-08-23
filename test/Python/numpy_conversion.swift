// RUN: %target-run-simple-swift
// REQUIRES: executable_test
//
// `numpy.ndarray` conversion tests.

import Python
import StdlibUnittest

var NumpyConversionTests = TestSuite("NumpyConversion")
let numpyModule = try? Python.attemptImport("numpy")

NumpyConversionTests.test("dtype-compatible") {
  guard let np = numpyModule else { return }
  expectTrue(Bool.isCompatible(withNumpyScalarType: np.bool))
  expectTrue(Bool.isCompatible(withNumpyScalarType: Python.bool))
  expectTrue(UInt8.isCompatible(withNumpyScalarType: np.uint8))
  expectTrue(Int8.isCompatible(withNumpyScalarType: np.int8))
  expectTrue(UInt16.isCompatible(withNumpyScalarType: np.uint16))
  expectTrue(Int16.isCompatible(withNumpyScalarType: np.int16))
  expectTrue(UInt32.isCompatible(withNumpyScalarType: np.uint32))
  expectTrue(Int32.isCompatible(withNumpyScalarType: np.int32))
  expectTrue(UInt64.isCompatible(withNumpyScalarType: np.uint64))
  expectTrue(Int64.isCompatible(withNumpyScalarType: np.int64))
  expectTrue(Float.isCompatible(withNumpyScalarType: np.float32))
  expectTrue(Float.isCompatible(withNumpyScalarType: np.single))
  expectTrue(Double.isCompatible(withNumpyScalarType: np.float64))
  expectTrue(Double.isCompatible(withNumpyScalarType: np.double))
}

NumpyConversionTests.test("array-conversion") {
  guard let np = numpyModule else { return }

  let numpyArrayEmpty = np.array([] as [Float], dtype: np.float32)
  if let array = expectNotNil(Array<Float>(numpyArray: numpyArrayEmpty)) {
    expectEqual([], array)
  }

  let numpyArrayBool = np.array([true, false, false, true])
  if let array = expectNotNil(Array<Bool>(numpyArray: numpyArrayBool)) {
    expectEqual([true, false, false, true], array)
  }

  let numpyArrayFloat = np.ones([6], dtype: np.float32)
  if let array = expectNotNil(Array<Float>(numpyArray: numpyArrayFloat)) {
    expectEqual(Array(repeating: 1, count: 6), array)
  }

  let numpyArrayInt32 = np.array([-1, 4, 25, 2018], dtype: np.int32)
  if let array = expectNotNil(Array<Int32>(numpyArray: numpyArrayInt32)) {
    expectEqual([-1, 4, 25, 2018], array)
  }

  let numpyArray2D = np.ones([2, 3])
  expectNil(Array<Float>(numpyArray: numpyArray2D))

  let numpyArrayStrided = np.array([[1, 2], [1, 2]], dtype: np.int32)[
      Python.slice(Python.None), 1]
  // Assert that the array has a stride, so that we're certainly testing a
  // strided array.
  expectNotEqual(numpyArrayStrided.__array_interface__["strides"], Python.None)
  if let array = expectNotNil(Array<Int32>(numpyArray: numpyArrayStrided)) {
    expectEqual([2, 2], array)
  }
}

runAllTests()
