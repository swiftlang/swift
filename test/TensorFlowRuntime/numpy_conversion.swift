// RUN: %target-run-eager-swift %swift-tensorflow-test-run-extra-options
// RUN: %target-run-gpe-swift %swift-tensorflow-test-run-extra-options
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
let ctypesModule = try? Python.attemptImport("ctypes")

NumpyConversionTests.test("shaped-array-conversion") {
  guard let np = numpyModule else { return }

  let numpyArrayEmpty = np.array([[]] as [[Float]], dtype: np.float32)
  if let array = expectNotNil(ShapedArray<Float>(numpy: numpyArrayEmpty)) {
    expectEqual(ShapedArray(shape: [1, 0], scalars: []), array)
  }

  let numpyArrayBool = np.array([[true, false], [false, true]])
  expectNil(ShapedArray<Int8>(numpy: numpyArrayBool))
  expectNil(ShapedArray<Float>(numpy: numpyArrayBool))
  if let array = expectNotNil(ShapedArray<Bool>(numpy: numpyArrayBool)) {
    expectEqual(ShapedArray(shape: [2, 2],
                            scalars: [true, false, false, true]),
                array)
  }

  let numpyArrayFloat = np.ones([2, 3], dtype: np.float32)
  expectNil(ShapedArray<Double>(numpy: numpyArrayFloat))
  expectNil(ShapedArray<Int32>(numpy: numpyArrayFloat))
  if let array = expectNotNil(ShapedArray<Float>(numpy: numpyArrayFloat)) {
    expectEqual(ShapedArray(repeating: 1, shape: [2, 3]), array)
  }

  let numpyArrayInt32 = np.array([[[1, 2, 3], [4, 5, 6]]], dtype: np.int32)
  expectNil(ShapedArray<Float>(numpy: numpyArrayInt32))
  expectNil(ShapedArray<UInt32>(numpy: numpyArrayInt32))
  if let array = expectNotNil(ShapedArray<Int32>(numpy: numpyArrayInt32)) {
    expectEqual(ShapedArray(shape: [1, 2, 3], scalars: [1, 2, 3, 4, 5, 6]),
                array)
  }

  let numpyArrayStrided = np.array([[1, 2], [1, 2]], dtype: np.int32)[
      Python.slice(Python.None), 1]
  // Assert that the array has a stride, so that we're certainly testing a
  // strided array.
  expectNotEqual(numpyArrayStrided.__array_interface__["strides"], Python.None)
  if let array = expectNotNil(
      ShapedArray<Int32>(numpy: numpyArrayStrided)) {
    expectEqual(ShapedArray(shape: [2], scalars: [2, 2]), array)
  }
}

NumpyConversionTests.test("tensor-conversion") {
  guard let np = numpyModule else { return }

  let numpyArrayEmpty = np.array([[]] as [[Float]], dtype: np.float32)
  if let tensor = expectNotNil(Tensor<Float>(numpy: numpyArrayEmpty)) {
    expectEqual(ShapedArray(shape: [1, 0], scalars: []), tensor.array)
  }

  let numpyArrayBool = np.array([[true, false], [false, true]])
  expectNil(Tensor<Int8>(numpy: numpyArrayBool))
  expectNil(Tensor<Float>(numpy: numpyArrayBool))
  if let tensor = expectNotNil(Tensor<Bool>(numpy: numpyArrayBool)) {
    expectEqual(ShapedArray(shape: [2, 2],
                            scalars: [true, false, false, true]),
                tensor.array)
  }

  let numpyArrayFloat = np.ones([2, 3], dtype: np.float32)
  expectNil(Tensor<Double>(numpy: numpyArrayFloat))
  expectNil(Tensor<Int32>(numpy: numpyArrayFloat))
  if let tensor = expectNotNil(Tensor<Float>(numpy: numpyArrayFloat)) {
    expectEqual(ShapedArray(repeating: 1, shape: [2, 3]), tensor.array)
  }

  let numpyArrayInt32 = np.array([[[1, 2, 3], [4, 5, 6]]], dtype: np.int32)
  expectNil(Tensor<Float>(numpy: numpyArrayInt32))
  expectNil(Tensor<UInt32>(numpy: numpyArrayInt32))
  if let tensor = expectNotNil(Tensor<Int32>(numpy: numpyArrayInt32)) {
    expectEqual(ShapedArray(shape: [1, 2, 3], scalars: [1, 2, 3, 4, 5, 6]),
                tensor.array)
  }

  let numpyArrayStrided = np.array([[1, 2], [1, 2]], dtype: np.int32)[
      Python.slice(Python.None), 1]
  // Assert that the array has a stride, so that we're certainly testing a
  // strided array.
  expectNotEqual(numpyArrayStrided.__array_interface__["strides"], Python.None)
  if let tensor = expectNotNil(Tensor<Int32>(numpy: numpyArrayStrided)) {
    expectEqual(ShapedArray(shape: [2], scalars: [2, 2]), tensor.array)
  }
}

NumpyConversionTests.test("tensor-round-trip") {
  guard numpyModule != nil else { return }
  guard ctypesModule != nil else { return }

  let t1 = Tensor<Float>(repeating: 3.0, shape: [1,2,3,4])
  expectEqual(t1, Tensor<Float>(numpy: t1.makeNumpyArray())!)

  let t2 = Tensor<UInt8>(shape: [2,3], scalars: [1, 2, 3, 4, 5, 6])
  expectEqual(t2, Tensor<UInt8>(numpy: t2.makeNumpyArray())!)

  let t3 = Tensor<Int32>(repeating: 30, shape: [8,5,4])
  expectEqual(t3, Tensor<Int32>(numpy: t3.makeNumpyArray())!)
}
#endif

runAllTests()
