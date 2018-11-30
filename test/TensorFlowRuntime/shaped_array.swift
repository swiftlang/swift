// RUN: %target-run-simple-swift %swift-tensorflow-test-run-extra-options
// RUN: %target-run-dynamic-compilation-swift %swift-tensorflow-test-run-extra-options
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize
//
// ShapedArray and ShapedArraySlice API tests.

import TensorFlow
import StdlibUnittest

// TODO(SR-7983): Investigate why this is necessary.
import SwiftOnoneSupport

var ShapedArrayTests = TestSuite("ShapedArrayTests")

// TODO: add full Collection scalar test suite.
// Putting off for now since API is not finalized and writing all the tests is
// time consuming.

ShapedArrayTests.test("AssociatedTypes") {
  expectRandomAccessCollectionAssociatedTypes(
    collectionType: ShapedArray<Float>.self,
    iteratorType: ShapedArray<Float>.Iterator.self,
    subSequenceType: ShapedArraySlice<Float>.self,
    indexType: Int.self,
    indicesType: Range<Int>.self
  )
}

ShapedArrayTests.test("Initializers") {
  let scalar = ShapedArray(shape: [], scalars: [1.0])
  expectEqual([], scalar.shape)
  expectEqual([1.0], scalar.scalars)

  let x = ShapedArray(shape: [2, 3], scalars: [1.0, 2.0, 3.0, 4.0, 5.0, 6.0])
  expectEqual([2, 3], x.shape)
  expectEqual([1.0, 2.0, 3.0, 4.0, 5.0, 6.0], x.scalars)

  let y: ShapedArraySlice<Double> = [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]
  expectEqual([2, 3], y.shape)
  expectEqual([1.0, 2.0, 3.0, 4.0, 5.0, 6.0], y.scalars)
}

ShapedArrayTests.test("Indexing") {
  let tensor = ShapedArray(shape: [3, 4, 5], scalars: Array(0..<60))

  /// Test shapes
  expectEqual([4, 5], tensor[0].shape)
  expectEqual([5], tensor[0][0].shape)
  expectEqual([], tensor[0][0][0].shape)
  expectEqual([2, 4, 5], tensor[0..<2].shape)
  expectEqual([3, 5], tensor[0][0..<3].shape)

  /// Test element tensor scalars
  expectEqual(Array(0..<20), tensor[0].scalars)
  expectEqual(Array(40..<60), tensor[2].scalars)
  expectEqual(Array(0..<5), tensor[0][0].scalars)
  expectEqual(Array(35..<40), tensor[1][3].scalars)
  expectEqual([43], tensor[2][0][3].scalars)
  expectEqual([37], tensor[1][3][2].scalars)

  /// Test subtensor scalars
  expectEqual(Array(20..<40), tensor[1..<2].scalars)
  expectEqual(Array(20..<30), tensor[1][0..<2].scalars)
  expectEqual(Array(45..<50), tensor[2][1..<2].scalars)
  expectEqual(Array(3..<5), tensor[0][0][3..<5].scalars)

  // TODO: add more indexing tests, such as slice of slice
}

ShapedArrayTests.test("ElementMutation") {
  var tensor = ShapedArray(shape: [3, 4, 5], scalars: Array(0..<60))
  tensor[0] = ShapedArraySlice<Int>(shape: [4, 5], repeating: 1)
  expectEqual(Array(repeating: 1, count: 20) + Array(20..<60), tensor.scalars)

  tensor[0..<2] = ShapedArraySlice<Int>(shape: [2, 4, 5], scalars: Array(0..<40))
  expectEqual(Array(0..<60), tensor.scalars)

  tensor[0][1..<3] = ShapedArraySlice<Int>(shape: [2, 5], scalars: Array(0..<10))
  expectEqual(Array(0..<5) + Array(0..<10) + Array(15..<60), tensor.scalars)

  for scalarIndex in tensor[0][0].indices {
    tensor[0][0][scalarIndex] = ShapedArraySlice<Int>(scalarIndex - 5)
  }
  expectEqual(Array(-5..<10) + Array(15..<60), tensor.scalars)
}

ShapedArrayTests.test("ScalarMutation") {
  var x = ShapedArray(shape: [2, 3], scalars: [0.0, 2.0, 3.0, 0.0, 0.0, 0.0])
  x.scalars[0] = 1.0
  expectEqual([1.0, 2.0, 3.0, 0.0, 0.0, 0.0], x.scalars)
  x.scalars[3...] = ArraySlice([4.0, 5.0, 6.0])
  expectEqual([1.0, 2.0, 3.0, 4.0, 5.0, 6.0], x.scalars)

  var y = ShapedArraySlice(shape: [2, 3], scalars: [0.0, 2.0, 3.0, 0.0, 0.0, 0.0])
  y.scalars[0] = 1.0
  expectEqual([1.0, 2.0, 3.0, 0.0, 0.0, 0.0], y.scalars)
  y.scalars[3...] = ArraySlice([4.0, 5.0, 6.0])
  expectEqual([1.0, 2.0, 3.0, 4.0, 5.0, 6.0], y.scalars)
}

ShapedArrayTests.test("StringDescription") {
  let scalar = ShapedArray(shape: [], scalars: [1.0])
  expectEqual("1.0", scalar.description)

  let x = ShapedArray(shape: [2, 3], scalars: [1.0, 2.0, 3.0, 4.0, 5.0, 6.0])
  expectEqual("[[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]", x.description)

  let y = ShapedArraySlice(shape: [2, 3], scalars: [1.0, 2.0, 3.0, 4.0, 5.0, 6.0])
  expectEqual("[[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]", y.description)
}

runAllTests()
