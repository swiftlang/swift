// RUN: %target-run-simple-swift
// REQUIRES: executable_test
//
// `numpy.ndarray` conversion tests.

import Python
import StdlibUnittest

var NumpyConversionTests = TestSuite("NumpyConversion")
let numpyModule = try? Python.attemptImport("numpy")

NumpyConversionTests.test("array-conversion") {
  guard let np = numpyModule else { return }

  let numpyArrayEmpty = np.array([] as [Float], dtype: np.float32)
  if let array = expectNotNil(Array<Float>(numpy: numpyArrayEmpty)) {
    expectEqual([], array)
  }

  let numpyArrayBool = np.array([true, false, false, true])
  if let array = expectNotNil(Array<Bool>(numpy: numpyArrayBool)) {
    expectEqual([true, false, false, true], array)
  }

  let numpyArrayFloat = np.ones([6], dtype: np.float32)
  if let array = expectNotNil(Array<Float>(numpy: numpyArrayFloat)) {
    expectEqual(Array(repeating: 1, count: 6), array)
  }

  let numpyArrayInt32 = np.array([-1, 4, 25, 2018], dtype: np.int32)
  if let array = expectNotNil(Array<Int32>(numpy: numpyArrayInt32)) {
    expectEqual([-1, 4, 25, 2018], array)
  }

  let numpyArray2D = np.ones([2, 3])
  expectNil(Array<Float>(numpy: numpyArray2D))

  let numpyArrayStrided = np.array([[1, 2], [1, 2]], dtype: np.int32)[
      Python.slice(Python.None), 1]
  // Assert that the array has a stride, so that we're certainly testing a
  // strided array.
  expectNotEqual(numpyArrayStrided.__array_interface__["strides"], Python.None)
  if let array = expectNotNil(Array<Int32>(numpy: numpyArrayStrided)) {
    expectEqual([2, 2], array)
  }
}

runAllTests()
