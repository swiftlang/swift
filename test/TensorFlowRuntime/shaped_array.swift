// RUN: %target-run-simple-swift
// REQUIRES: executable_test
//
// ShapedArray and ShapedArraySlice API tests.

import TensorFlow
import StdlibUnittest

var ShapedArrayTests = TestSuite("ShapedArrayTests")

// TODO: add full Collection scalar test suite.
// Putting off for now since API is not finalized and writing all the tests is
// time consuming.

ShapedArrayTests.test("Initializers") {
  let scalar = ShapedArray(shape: [], scalars: [1.0])
  expectEqual(scalar.shape, [])
  expectEqual(scalar.scalars, [1.0])

  let x = ShapedArray(shape: [2, 3], scalars: [1.0, 2.0, 3.0, 4.0, 5.0, 6.0])
  expectEqual(x.shape, [2, 3])
  expectEqual(x.scalars, [1.0, 2.0, 3.0, 4.0, 5.0, 6.0])

  let y = ShapedArraySlice(shape: [2, 3], scalars: [1.0, 2.0, 3.0, 4.0, 5.0, 6.0])
  expectEqual(y.shape, [2, 3])
  expectEqual(y.scalars, [1.0, 2.0, 3.0, 4.0, 5.0, 6.0])
}

ShapedArrayTests.test("Indexing") {
  var tensor = ShapedArray(shape: [3, 4, 5], scalars: Array(0..<60))

  /// Test shapes
  expectEqual(tensor[0].shape, [4, 5])
  expectEqual(tensor[0][0].shape, [5])
  expectEqual(tensor[0][0][0].shape, [])
  expectEqual(tensor[0..<2].shape, [2, 4, 5])
  expectEqual(tensor[0][0..<3].shape, [3, 5])

  /// Test element tensor indexing
  expectEqual(tensor[0].scalars, Array(0..<20))
  expectEqual(tensor[2].scalars, Array(40..<60))
  expectEqual(tensor[0][0].scalars, Array(0..<5))
  expectEqual(tensor[1][3].scalars, Array(35..<40))
  expectEqual(tensor[2][0][3].scalars, [43])
  expectEqual(tensor[1][3][2].scalars, [37])

  /// Test subtensor indexing
  expectEqual(tensor[2..<3].scalars, Array(40..<60))
  expectEqual(tensor[1][0..<2].scalars, Array(20..<30))
  expectEqual(tensor[2][1..<2].scalars, Array(45..<50))
  expectEqual(tensor[0][0][3..<5].scalars, Array(3..<5))
}

ShapedArrayTests.test("ElementMutation") {
  var tensor = ShapedArray(shape: [3, 4, 5], scalars: Array(0..<60))
  tensor[0] = ShapedArraySlice<Int>(shape: [4, 5], repeating: 1)
  expectEqual(tensor.scalars, Array(repeating: 1, count: 20) + Array(20..<60))
  tensor[0..<2] = ShapedArraySlice<Int>(shape: [2, 4, 5], scalars: Array(0..<40))
  expectEqual(tensor.scalars, Array(0..<60))
  tensor[0][1..<3] = ShapedArraySlice<Int>(shape: [2, 5], scalars: Array(0..<10))
  expectEqual(tensor.scalars, Array((0..<5)) + Array((0..<10)) + Array(15..<60))
  for scalarIndex in tensor[0][0].indices {
    tensor[0][0][scalarIndex] = ShapedArraySlice<Int>(scalarIndex - 5)
  }
  expectEqual(tensor.scalars, Array((-5..<10)) + Array(15..<60))
}

ShapedArrayTests.test("ScalarMutation") {
  var x = ShapedArray(shape: [2, 3], scalars: [0.0, 2.0, 3.0, 0.0, 0.0, 0.0])
  x.scalars[0] = 1.0
  expectEqual(x.scalars, [1.0, 2.0, 3.0, 0.0, 0.0, 0.0])
  x.scalars[3...] = ArraySlice([4.0, 5.0, 6.0])
  expectEqual(x.scalars, [1.0, 2.0, 3.0, 4.0, 5.0, 6.0])

  var y = ShapedArraySlice(shape: [2, 3], scalars: [0.0, 2.0, 3.0, 0.0, 0.0, 0.0])
  y.scalars[0] = 1.0
  expectEqual(y.scalars, [1.0, 2.0, 3.0, 0.0, 0.0, 0.0])
  y.scalars[3...] = ArraySlice([4.0, 5.0, 6.0])
  expectEqual(y.scalars, [1.0, 2.0, 3.0, 4.0, 5.0, 6.0])
}

ShapedArrayTests.test("AssociatedTypes") {
  expectRandomAccessCollectionAssociatedTypes(
    collectionType: ShapedArray<Float>.self,
    iteratorType: ShapedArray<Float>.Iterator.self,
    subSequenceType: ShapedArraySlice<Float>.self,
    indexType: Int.self,
    indexDistanceType: Int.self,
    indicesType: CountableRange<Int>.self)
}

ShapedArrayTests.test("StringDescription") {
  let scalar = ShapedArray(shape: [], scalars: [1.0])
  expectEqual(scalar.description, "1.0")

  let x = ShapedArray(shape: [2, 3], scalars: [1.0, 2.0, 3.0, 4.0, 5.0, 6.0])
  expectEqual(x.description, "[[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]")

  let y = ShapedArraySlice(shape: [2, 3], scalars: [1.0, 2.0, 3.0, 4.0, 5.0, 6.0])
  expectEqual(y.description, "[[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]")
}

runAllTests()
