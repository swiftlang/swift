// RUN: %target-run-simple-swift
// REQUIRES: executable_test
//
// Infeed tests.

import TensorFlow
#if TPU
import TestUtilsTPU
#else
import TestUtils
#endif
import StdlibUnittest

var InfeedTests = TestSuite("Infeed")

@inline(never)
func testScalarInput() {
  func add(_ x: Float, _ y: Float) -> Float {
    let x = Tensor<Float>(x)
    let y = Tensor<Float>(y)
    return (x+y).array.scalars[0]
  }

  _RuntimeConfig.executionMode = .tpu(usesInfeed: true)
  expectNearlyEqual(3.7, add(1.3, 2.4), byError: 0.1)
}
InfeedTests.testTPU("ScalarInput", testScalarInput)

#if false
// TODO(hongm): Extend shape info support to make this test work.
// TODO(hongm): Unify with TensorTests.ElementIndexing
InfeedTests.testTPU("ElementIndexing") {
  _RuntimeConfig.executionMode = .tpu(usesInfeed: true)

  // NOTE: This tests the `subscript(index:)` method, which is distinct from
  // the `subscript(indices:)` method.
  // NOTE: cannot test multiple `Tensor.shape` or `Tensor.scalars` directly
  // until send and receive are implemented (without writing a bunch of mini
  // tests). Instead, `Tensor.array` is called to make a ShapedArray host copy
  // and the ShapedArray is tested.
  let tensor3D = Tensor<Float>(shape: [3, 4, 5],
                               scalars: Array(stride(from: 0.0, to: 60, by: 1)))
  let element2D = tensor3D[2]
  let element1D = tensor3D[1][3]
  let element0D = tensor3D[2][0][3]

  let array2D = element2D.array
  let array1D = element1D.array
  let array0D = element0D.array

  /// Test shapes
  expectEqual([4, 5], array2D.shape)
  expectEqual([5], array1D.shape)
  expectEqual([], array0D.shape)

  /// Test scalars
  expectEqual(Array(stride(from: 40.0, to: 60, by: 1)), array2D.scalars)
  expectEqual(Array(stride(from: 35.0, to: 40, by: 1)), array1D.scalars)
  expectEqual([43], array0D.scalars)  
}
#endif

runAllTests()
