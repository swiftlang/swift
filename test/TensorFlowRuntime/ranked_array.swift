// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize
//
// ArrayXD and ArraySliceXD API tests.

import TensorFlow
import StdlibUnittest

var RankedArrayTests = TestSuite("RankedArrayTests")

// TODO: add full Collection scalar test suite.
// Putting off for now since API is not finalized and writing all the tests is
// time consuming.

RankedArrayTests.test("AssociatedTypes") {
  expectRandomAccessCollectionAssociatedTypes(
    collectionType: Array1D<Float>.self,
    iteratorType: Array1D<Float>.Iterator.self,
    subSequenceType: ArraySlice1D<Float>.self,
    indexType: Int.self,
    indicesType: Range<Int>.self
  )

  expectRandomAccessCollectionAssociatedTypes(
    collectionType: Array2D<Float>.self,
    iteratorType: Array2D<Float>.Iterator.self,
    subSequenceType: ArraySlice2D<Float>.self,
    indexType: Int.self,
    indicesType: Range<Int>.self
  )
}

RankedArrayTests.test("Initializers") {
  let array1D: Array1D = [1.0, 2.0, 3.0]
  expectEqual([1.0, 2.0, 3.0], array1D)

  let slice1D: ArraySlice1D = [1.0, 2.0, 3.0]
  expectEqual([1.0, 2.0, 3.0], slice1D)

  let array2D = Array2D(shape: [2, 3], scalars: [1.0, 2.0, 3.0, 4.0, 5.0, 6.0])
  expectEqual(2, array2D.rank)
  expectEqual([2, 3], array2D.shape)
  expectEqual([1.0, 2.0, 3.0, 4.0, 5.0, 6.0], array2D.scalars)

  let slice2D = ArraySlice2D(shape: [2, 3], scalars: [1.0, 2.0, 3.0, 4.0, 5.0, 6.0])
  expectEqual(2, slice2D.rank)
  expectEqual([2, 3], slice2D.shape)
  expectEqual([1.0, 2.0, 3.0, 4.0, 5.0, 6.0], slice2D.scalars)
}

RankedArrayTests.test("Indexing") {
  var tensor = Array3D(shape: [3, 4, 5], scalars: Array(0..<60))

  /// Test shapes
  expectEqual([4, 5], tensor[0].shape)
  expectEqual(5, tensor[0][0].count)
  expectEqual([2, 4, 5], tensor[0..<2].shape)
  expectEqual([3, 5], tensor[0][0..<3].shape)

  /// Test element tensor indexing
  expectEqual(Array(0..<20), tensor[0].scalars)
  expectEqual(Array(40..<60), tensor[2].scalars)
  expectEqual(ArraySlice(0..<5), tensor[0][0])
  expectEqual(ArraySlice(35..<40), tensor[1][3])
  expectEqual(43, tensor[2][0][3])
  expectEqual(37, tensor[1][3][2])

  /// Test subtensor indexing
  expectEqual(Array(40..<60), tensor[2..<3].scalars)
  expectEqual(Array(20..<30), tensor[1][0..<2].scalars)
  expectEqual(Array(45..<50), tensor[2][1..<2].scalars)
  expectEqual(ArraySlice(3..<5), tensor[0][0][3..<5])
}

RankedArrayTests.test("ElementMutation") {
  var tensor = Array3D(shape: [3, 4, 5], scalars: Array(0..<60))
  tensor[0] = ArraySlice2D<Int>(shape: [4, 5], repeating: 1)
  expectEqual(Array(repeating: 1, count: 20) + Array(20..<60), tensor.scalars)

  tensor[0..<2] = ArraySlice3D<Int>(shape: [2, 4, 5], scalars: Array(0..<40))
  expectEqual(Array(0..<60), tensor.scalars)

  tensor[0][1..<3] = ArraySlice2D<Int>(shape: [2, 5], scalars: Array(0..<10))
  expectEqual(Array(0..<5) + Array(0..<10) + Array(15..<60), tensor.scalars)

  for scalarIndex in tensor[0][0].indices {
    tensor[0][0][scalarIndex] = scalarIndex - 5
  }
  expectEqual(Array(-5..<10) + Array(15..<60), tensor.scalars)
}

RankedArrayTests.test("ScalarMutation") {
  var x = Array2D(shape: [2, 3], scalars: [0.0, 2.0, 3.0, 0.0, 0.0, 0.0])
  x.scalars[0] = 1.0
  expectEqual([1.0, 2.0, 3.0, 0.0, 0.0, 0.0], x.scalars)
  x.scalars[3...] = ArraySlice([4.0, 5.0, 6.0])
  expectEqual([1.0, 2.0, 3.0, 4.0, 5.0, 6.0], x.scalars)

  var y = ArraySlice2D(shape: [2, 3], scalars: [0.0, 2.0, 3.0, 0.0, 0.0, 0.0])
  y.scalars[0] = 1.0
  expectEqual([1.0, 2.0, 3.0, 0.0, 0.0, 0.0], y.scalars)
  y.scalars[3...] = ArraySlice([4.0, 5.0, 6.0])
  expectEqual([1.0, 2.0, 3.0, 4.0, 5.0, 6.0], y.scalars)
}

RankedArrayTests.test("StringDescription") {
  let array1D: Array1D<Float> = [1.0, 2.0, 3.0, 4.0, 5.0]
  expectEqual("[1.0, 2.0, 3.0, 4.0, 5.0]", array1D.description)

  let slice1D: ArraySlice1D<Float> = [1.0, 2.0, 3.0, 4.0, 5.0]
  expectEqual("[1.0, 2.0, 3.0, 4.0, 5.0]", slice1D.description)

  let array2D = Array2D(shape: [2, 3], scalars: [1.0, 2.0, 3.0, 4.0, 5.0, 6.0])
  expectEqual("[[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]", array2D.description)

  let slice2D = ArraySlice2D(shape: [2, 3], scalars: [1.0, 2.0, 3.0, 4.0, 5.0, 6.0])
  expectEqual("[[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]", slice2D.description)
}

runAllTests()
