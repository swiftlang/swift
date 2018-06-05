// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize
//
// `numpy.ndarray` conversion tests.
// `swift_test_mode_optimize` is required for `Tensor` conversions.

import Python
import StdlibUnittest

var NumpyArrayConversionTests = TestSuite("NumpyArrayConversion")
let numpyModule = try? Python.attemptImport("numpy")

NumpyArrayConversionTests.test("dtype-compatible") {
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

NumpyArrayConversionTests.test("array-conversion") {
  guard let np = numpyModule else { return }

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
}

#if canImport(TensorFlow)
import TensorFlow

NumpyArrayConversionTests.test("shapedarray-conversion") {
  guard let np = numpyModule else { return }

  let numpyArrayBool = np.array([[true, false], [false, true]])
  expectNil(ShapedArray<Int8>(numpyArray: numpyArrayBool))
  expectNil(ShapedArray<Float>(numpyArray: numpyArrayBool))
  if let array = expectNotNil(ShapedArray<Bool>(numpyArray: numpyArrayBool)) {
    expectEqual(ShapedArray(shape: [2, 2],
                            scalars: [true, false, false, true]),
                array)
  }

  let numpyArrayFloat = np.ones([2, 3], dtype: np.float32)
  expectNil(ShapedArray<Double>(numpyArray: numpyArrayFloat))
  expectNil(ShapedArray<Int32>(numpyArray: numpyArrayFloat))
  if let array = expectNotNil(ShapedArray<Float>(numpyArray: numpyArrayFloat)) {
    expectEqual(ShapedArray(shape: [2, 3], repeating: 1), array)
  }

  let numpyArrayInt32 = np.array([[[1, 2, 3], [4, 5, 6]]], dtype: np.int32)
  expectNil(ShapedArray<Float>(numpyArray: numpyArrayInt32))
  expectNil(ShapedArray<UInt32>(numpyArray: numpyArrayInt32))
  if let array = expectNotNil(ShapedArray<Int32>(numpyArray: numpyArrayInt32)) {
    expectEqual(ShapedArray(shape: [1, 2, 3], scalars: [1, 2, 3, 4, 5, 6]),
                array)
  }
}

NumpyArrayConversionTests.test("tensor-conversion") {
  guard let np = numpyModule else { return }

  let numpyArrayBool = np.array([[true, false], [false, true]])
  expectNil(Tensor<Int8>(numpyArray: numpyArrayBool))
  expectNil(Tensor<Float>(numpyArray: numpyArrayBool))
  if let tensor = expectNotNil(Tensor<Bool>(numpyArray: numpyArrayBool)) {
    expectEqual(ShapedArray(shape: [2, 2],
                            scalars: [true, false, false, true]),
                tensor.array)
  }

  let numpyArrayFloat = np.ones([2, 3], dtype: np.float32)
  expectNil(Tensor<Double>(numpyArray: numpyArrayFloat))
  expectNil(Tensor<Int32>(numpyArray: numpyArrayFloat))
  if let tensor = expectNotNil(Tensor<Float>(numpyArray: numpyArrayFloat)) {
    expectEqual(ShapedArray(shape: [2, 3], repeating: 1), tensor.array)
  }

  let numpyArrayInt32 = np.array([[[1, 2, 3], [4, 5, 6]]], dtype: np.int32)
  expectNil(Tensor<Float>(numpyArray: numpyArrayInt32))
  expectNil(Tensor<UInt32>(numpyArray: numpyArrayInt32))
  if let tensor = expectNotNil(Tensor<Int32>(numpyArray: numpyArrayInt32)) {
    expectEqual(ShapedArray(shape: [1, 2, 3], scalars: [1, 2, 3, 4, 5, 6]),
                tensor.array)
  }
}
#endif

runAllTests()
