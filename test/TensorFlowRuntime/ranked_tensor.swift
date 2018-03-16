// RUN: %target-run-simple-swift
// REQUIRES: executable_test
//
// Tensor API tests.

import TensorFlow
#if TPU
import TestUtilsTPU
#else
import TestUtils
#endif
import StdlibUnittest

var RankedTensorTests = TestSuite("RankedTensor")

RankedTensorTests.testAllBackends("Initializers") {
  let vector = Tensor1D<Float>([1.0, 2.0, 3.0, 4.0, 5.0, 6.0])
  let matrix = Tensor2D<Float>([[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]])
  let tensor = Tensor3D(identicallyRanked: Tensor<Float>(ones: [2, 3, 4]))
  let broadcasted4DScalar = Tensor4D<Float>(broadcasting: 4)
  expectEqual([1, 2, 3, 4, 5, 6], vector.array)
  expectEqual(Array2D(shape: [2, 3], scalars: [1, 2, 3, 4, 5, 6]), matrix.array)
  expectEqual(Array3D(shape: [2, 3, 4], repeating: 1), tensor.array)
  expectEqual(Array4D(shape: [1, 1, 1, 1], scalars: [4]),
              broadcasted4DScalar.array)
}

RankedTensorTests.testAllBackends("FactoryInitializers") {
  let x = Tensor2D<Float>(ones: [1, 10])
  expectEqual([1, 10], x.shape)
  expectEqual(Array(repeating: 1, count: 10), x.scalars)
}

RankedTensorTests.testAllBackends("ScalarToTensorConversion1") {
  let matrix = Tensor2D<Float>(broadcasting: -1)
  expectEqual([1, 1], matrix.shape)
  expectEqual([-1], matrix.scalars)
}

// TODO: Merge into the previous example when we support code motion to avoid
// sends.
RankedTensorTests.testAllBackends("ScalarToTensorConversion2") {
  let tensor = Tensor4D<Float>(broadcasting: 42)
  expectEqual([1, 1, 1, 1], tensor.shape)
  expectEqual([42], tensor.scalars)
}

RankedTensorTests.testAllBackends("ArrayXDConversion") {
  let array3D = Array3D(shape: [2, 3, 4], repeating: 1.0)
  let tensor3D = Tensor3D(array3D)
  expectEqual(array3D, tensor3D.array)
}

RankedTensorTests.testAllBackends("DataTypeCast") {
  // TPU does not support DT_INT8 or 16 for cast
  guard _RuntimeConfig.executionMode != .tpu else { return }

  let x = Tensor2D<Int32>(ones: [5, 5])
  let ints = Tensor2D<Int64>(x)
  let floats = Tensor2D<Float>(x)
  let i8s = Tensor2D<Int8>(floats)
  expectEqual(Array2D(shape: [5, 5], repeating: 1), ints.array)
  expectEqual(Array2D(shape: [5, 5], repeating: 1), floats.array)
  expectEqual(Array2D(shape: [5, 5], repeating: 1), i8s.array)
}

RankedTensorTests.testAllBackends("BoolToNumericCast") {
  guard _RuntimeConfig.executionMode != .tpu else { return }

  let bools = Tensor2D<Bool>([[true, false], [true, false]])
  let ints = Tensor2D<Int64>(bools)
  let floats = Tensor2D<Float>(bools)
  let i8s = Tensor2D<Int8>(bools)
  expectEqual(Array2D(shape: [2, 2], scalars: [1, 0, 1, 0]), ints.array)
  expectEqual(Array2D(shape: [2, 2], scalars: [1, 0, 1, 0]), floats.array)
  expectEqual(Array2D(shape: [2, 2], scalars: [1, 0, 1, 0]), i8s.array)
}

RankedTensorTests.test("ElementIndexing") {
  // XLA compilation error under TPU.
  guard _RuntimeConfig.executionMode != .tpu else { return }

  // NOTE: This tests the `subscript(index:)` method, which is distinct from
  // the `subscript(indices:)` method.
  // NOTE: cannot test multiple `Tensor.shape` or `Tensor.scalars` directly
  // until send and receive are implemented (without writing a bunch of mini
  // tests). Instead, `Tensor.array` is called to make a ShapedArray host copy
  // and the ShapedArray is tested.
  let tensor3D = Tensor3D<Float>(
    // TODO: We should add tensor initializers for common things like this so
    // they don't have to be copied to the device.
    shape: [3, 4, 5], scalars: Array(stride(from: 0.0, to: 60, by: 1))
  ).toDevice()
  let element2D = tensor3D[2]
  let element1D = tensor3D[1][3]
  let element0D = tensor3D[2][0][3]

  let array2D = element2D.array
  let array1D = element1D.array

  /// Test shapes
  expectEqual([4, 5], array2D.shape)
  expectEqual(5, array1D.count)

  /// Test scalars
  expectEqual(Array(stride(from: 40.0, to: 60.0, by: 1)), array2D.scalars)
  expectEqual(Array(stride(from: 35.0, to: 40.0, by: 1)), array1D)
  expectEqual(43, element0D)
}

RankedTensorTests.test("SliceIndexing") {
  // XLA compilation error under TPU.
  guard _RuntimeConfig.executionMode != .tpu else { return }

  // NOTE: cannot test `TensorXD.shape` or `TensorXD.scalars` directly until
  // send and receive are implemented (without writing a bunch of mini tests).
  // Instead, `TensorXD.array` is called to make an ArrayXD host copy and the
  // ArrayXD is tested instead.
  let tensor3D = Tensor3D<Float>(
    shape: [3, 4, 5], scalars: Array(stride(from: 0.0, to: 60, by: 1))
  ).toDevice()

  let slice3D = tensor3D[1..<2]
  let slice2D = tensor3D[1][0..<2]
  let slice1D = tensor3D[0][0][3..<5]

  let array3D = slice3D.array
  let array2D = slice2D.array
  let array1D = slice1D.array

  /// Test shapes
  expectEqual([1, 4, 5], array3D.shape)
  expectEqual([2, 5], array2D.shape)
  expectEqual(2, array1D.count)

  /// Test scalars
  expectEqual(Array(stride(from: 20.0, to: 40, by: 1)), array3D.scalars)
  expectEqual(Array(stride(from: 20.0, to: 30, by: 1)), array2D.scalars)
  expectEqual(Array(stride(from: 3.0, to: 5, by: 1)), array1D)
}

RankedTensorTests.testAllBackends("Reduction") {
  // 2 x 5
  let x = Tensor2D<Float>([[1, 2, 3, 4, 5], [1, 2, 3, 4, 5]])
  let sum = x.sum(squeezingAxis: 0)
  expectEqual([2, 4, 6, 8, 10], sum.array)
}

RankedTensorTests.testAllBackends("Concatenation") {
  // 2 x 3
  let t1 = Tensor2D<Int32>([[0, 1, 2], [3, 4, 5]])
  // 2 x 3
  let t2 = Tensor2D<Int32>([[6, 7, 8], [9, 10, 11]])
  let concatenated = t1 ++ t2
  let concatenated0 = t1.concatenated(with: t2)
  let concatenated1 = t1.concatenated(with: t2, alongAxis: 1)
  expectEqual(Array2D(shape: [4, 3], scalars: Array(0..<12)),
              concatenated.array)
  expectEqual(Array2D(shape: [4, 3], scalars: Array(0..<12)),
              concatenated0.array)
  expectEqual(Array2D(shape: [2, 6],
                      scalars: [0, 1, 2, 6, 7, 8, 3, 4, 5, 9, 10, 11]),
              concatenated1.array)
}

RankedTensorTests.testAllBackends("ArgMax") {
  // 2 x 3
  let x = Tensor2D<Float>([[0, 1, 2], [3, 4, 5]])
  let argmax0 = x.argmax(squeezingAxis: 0)
  let argmax1 = x.argmax(squeezingAxis: 1)
  let scalarsArgmax = x.argmax()
  expectEqual([1, 1, 1], argmax0.array)
  expectEqual([2, 2], argmax1.array)
  expectEqual(5, scalarsArgmax)
}

RankedTensorTests.testAllBackends("SimpleMath") {
  let x = Tensor1D<Float>([1.2, 1.2])
  let y = tanh(x)
  expectPointwiseNearlyEqual([0.833655, 0.833655], y.array, byError: 0.0001)
}

// NOTE: scalar-tensor ops are tested for TensorXD but not Tensor because the
// way such ops are resolved differs. s+t ops for Tensor call the underlying t+t
// implementation, but s+t ops for TensorXD call the true s+t implementation
// defined on TensorProtocol.
// TODO: A more comprehensive way to eliminate redundant tests is to define
// generic functions on TensorProtocol and call those.
RankedTensorTests.testAllBackends("ScalarTensorOps1") {
  let scalar: Float = 2.0
  let tensor = Tensor2D<Float>(ones: [2, 3])
  let sum = tensor + scalar
  let diff = scalar - tensor

  let sumArray = sum.array
  let diffArray = diff.array
  expectEqual(Array2D(shape: [2, 3], scalars: [3, 3, 3, 3, 3, 3]),
              sumArray)
  expectEqual(Array2D(shape: [2, 3], scalars: [1, 1, 1, 1, 1, 1]),
              diffArray)
}

RankedTensorTests.testAllBackends("ScalarTensorOps2") {
  let scalar: Float = 2.0
  var tensor = Tensor2D<Float>(shape: [2, 3], repeating: 5)
  tensor += scalar
  tensor -= scalar
  tensor *= scalar
  tensor /= scalar

  expectEqual(Array2D(shape: [2, 3], scalars: [5, 5, 5, 5, 5, 5]),
              tensor.array)
}

RankedTensorTests.testAllBackends("Convolution") {
  // TODO: the code for initializing Tensor4D instances here is quite verbose.
  // Consider adding `init(shape:repeating)` and/or `init(shape:scalars)`
  // initializers to TensorXD?
  let x = Tensor4D<Float>(shape: [1, 1, 3, 3], repeating: 0.5)
  let filter = Tensor4D<Float>(shape: [1, 1, 3, 3],
                               scalars: [0, 1, 0, 1, 1, 1, 0, 1, 0])
  let y = x.convolved2D(withFilter: filter, strides: (1, 1, 1, 1),
                        padding: .same)
  expectEqual(Array4D(shape: [1, 1, 3, 3],
                      scalars: [0.5, 1.5, 0.5,
                                0.5, 1.5, 0.5,
                                0.5, 1.5, 0.5]),
              y.array)
}

RankedTensorTests.testAllBackends("3Adds") {
  let a = Tensor1D<Float>([1])
  let b = Tensor1D<Float>([2])
  let c = Tensor1D<Float>([3])
  let o = a + b + c

  expectEqual([6], o.array)
}

RankedTensorTests.testAllBackends("testMultiOpMath") {
  let x = Tensor1D<Float>([1.2, 1.2])
  let y = Tensor1D<Float>([2.4, 2.4])
  let t1 = x + y
  let t2 = t1 * t1
  let t3 = sqrt(t2)

  expectPointwiseNearlyEqual([3.6, 3.6], t1.array)
  expectPointwiseNearlyEqual([12.96, 12.96], t2.array)
  expectPointwiseNearlyEqual([3.6, 3.6], t3.array)
}

RankedTensorTests.testAllBackends("testXWPlusB") {
  // Shape: 1 x 4
  let x = Tensor2D<Float>([[1.0, 2.0, 2.0, 1.0]])
  // Shape: 4 x 2
  let w = Tensor2D<Float>([[1.0, 0.0], [3.0, 0.0], [2.0, 3.0], [1.0, 0.0]])
  // Shape: 2
  let b = Tensor1D<Float>([0.5, 0.5])
  // Do xW+b!
  let result = x ⊗ w + b.rankLifted()
  expectEqual([1, 2], result.shape)
  expectPointwiseNearlyEqual([12.5, 6.5], result.scalars)
}

RankedTensorTests.testAllBackends("Transpose") {
  // Shape: 3 x 2
  let xT = Tensor2D<Float>([[1, 2], [3, 4], [5, 6]]).transposed()
  expectEqual(2, xT.rank)
  expectEqual([2, 3], xT.shape)
  expectEqual([1, 3, 5, 2, 4, 6], xT.scalars)
}

RankedTensorTests.testAllBackends("Flatten") {
  // 2 x 3
  let matrix = Tensor2D<Int32>([[0, 1, 2], [3, 4, 5]])
  let flattened = matrix.flattened()
  expectEqual([6], flattened.shape)
  expectEqual(Array(0..<6), flattened.scalars)
}

// FIXME: Partitioner bug (b/72997202)
#if false // Remove #if when fixed.
// FIXME: The While op doesn't work on the CPU.
RankedTensorTests.testGPU("simpleCounterLoop") {
  let maxCount = 100
  var a = Tensor1D<Int32>(0)
  let b = Tensor1D<Int32>(1)
  var count = 0

  a -= b
  while count < maxCount {
    a += b
    count += 1
  }
  a -= b
  expectEqual([8], a.scalars)
}
#endif

@inline(never)
func testXORInference() {
  func xor(_ x: Float, _ y: Float) -> Float {
    let x = Tensor2D<Float>([[x, y]])

    // FIXME: If params are declared outside of `xor`, it would crash.
    // 2 x 4
    let w1 = Tensor2D<Float>(
      [[-1.83586664, -0.20809225, 0.47667537, 1.90780607],
       [-1.83523219, -0.51167348, 0.15490439, 1.91018065]])
    // 1 x 4
    let b1 = Tensor2D<Float>(
      [[2.54353216, 0.25132703, -0.16503136, -0.85754058]])
    // 4 x 1
    let w2 = Tensor2D<Float>(
      [[3.04350065], [0.35590511], [-0.3252157], [3.49349223]])
    // 1 x 1
    let b2 = Tensor2D<Float>([[-0.74635993]])

    let o1 = tanh(x ⊗ w1 + b1)
    let y = tanh(o1 ⊗ w2 + b2)
    return y.scalars[0]
  }
  expectNearlyEqual(0.0, xor(0.0, 0.0), byError: 0.1)
  expectNearlyEqual(1.0, xor(0.0, 1.0), byError: 0.1)
  expectNearlyEqual(1.0, xor(1.0, 0.0), byError: 0.1)
  expectNearlyEqual(0.0, xor(1.0, 1.0), byError: 0.1)
}
RankedTensorTests.testAllBackends("XORInference", testXORInference)

runAllTests()
