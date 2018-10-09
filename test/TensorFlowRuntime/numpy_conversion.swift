// RUN: %target-run-simple-swift
// RUN: %target-run-dynamic-compilation-swift
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize
// REQUIRES: tensorflow
//
// `numpy.ndarray` conversion tests.

import TensorFlow
import StdlibUnittest

var NumpyConversionTests = TestSuite("NumpyConversion")

// TODO: Add `python` as a lit feature so this test can use "REQUIRE: python"
// instead of `#if canImport(Python)`.

#if canImport(Python)
import Python

let numpyModule = try? Python.attemptImport("numpy")

NumpyConversionTests.test("shaped-array-conversion") {
  guard let np = numpyModule else { return }

  let numpyArrayEmpty = np.array([[]] as [[Float]], dtype: np.float32)
  if let array = expectNotNil(ShapedArray<Float>(numpyArray: numpyArrayEmpty)) {
    expectEqual(ShapedArray(shape: [1, 0], scalars: []), array)
  }

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

  let numpyArrayStrided = np.array([[1, 2], [1, 2]], dtype: np.int32)[
      Python.slice(Python.None), 1]
  // Assert that the array has a stride, so that we're certainly testing a
  // strided array.
  expectNotEqual(numpyArrayStrided.__array_interface__["strides"], Python.None)
  if let array = expectNotNil(
      ShapedArray<Int32>(numpyArray: numpyArrayStrided)) {
    expectEqual(ShapedArray(shape: [2], scalars: [2, 2]), array)
  }
}

NumpyConversionTests.test("tensor-conversion") {
  guard let np = numpyModule else { return }

  let numpyArrayEmpty = np.array([[]] as [[Float]], dtype: np.float32)
  if let tensor = expectNotNil(Tensor<Float>(numpyArray: numpyArrayEmpty)) {
    expectEqual(ShapedArray(shape: [1, 0], scalars: []), tensor.array)
  }

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

  let numpyArrayStrided = np.array([[1, 2], [1, 2]], dtype: np.int32)[
      Python.slice(Python.None), 1]
  // Assert that the array has a stride, so that we're certainly testing a
  // strided array.
  expectNotEqual(numpyArrayStrided.__array_interface__["strides"], Python.None)
  if let tensor = expectNotNil(Tensor<Int32>(numpyArray: numpyArrayStrided)) {
    expectEqual(ShapedArray(shape: [2], scalars: [2, 2]), tensor.array)
  }
}
#endif

runAllTests()
