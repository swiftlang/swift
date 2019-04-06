// RUN: %target-run-eager-swift %swift-tensorflow-test-run-extra-options
// RUN: %target-run-gpe-swift %swift-tensorflow-test-run-extra-options
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize
//
// `Tensor` string description tests.

import TensorFlow
import StdlibUnittest

// Note: Foundation is needed for `String.contains(_: String)`.
#if canImport(Foundation)
import Foundation
#endif

var StringDescriptionTests = TestSuite("StringDescriptionTests")

StringDescriptionTests.test("Empty") {
  let empty = Tensor<Float>([] as [Float])
  expectEqual("[]", empty.description)
}

StringDescriptionTests.test("Scalar") {
  do {
    let scalar = Tensor<Int32>(1)
    expectEqual("1", scalar.description)
  }

  do {
    let scalar = Tensor<Float>(-3.14)
    expectEqual("-3.14", scalar.description)
  }
}

StringDescriptionTests.test("Vector") {
  do {
    let vector = Tensor<Int32>(ones: [4])
    expectEqual("[1, 1, 1, 1]", vector.description)
  }

  do {
    var vector = Tensor<Float>([1, 2, 3, 4])
    expectEqual("[1.0, 2.0, 3.0, 4.0]", vector.description)
    vector[1] = Tensor<Float>(-2)
    expectEqual("[ 1.0, -2.0,  3.0,  4.0]", vector.description)
  }

  // Test long vector (above 1000 scalar threshold).
  do {
    let vector = Tensor<Float>(repeating: 3, shape: [1001])
    expectEqual("[3.0, 3.0, 3.0, ..., 3.0, 3.0, 3.0]", vector.description)
  }
}

StringDescriptionTests.test("Matrix") {
  do {
    var matrix = Tensor<Int32>(ones: [2, 2])
    expectEqual("""
      [[1, 1],
       [1, 1]]
      """, matrix.description)
    // Increase max scalar length.
    // Check that scalars are still aligned (properly left padded).
    matrix[0][1] = Tensor(-1)
    expectEqual("""
      [[ 1, -1],
       [ 1,  1]]
      """, matrix.description)
  }

  do {
    var matrix = Tensor<Float>(ones: [2, 2])
    expectEqual("""
      [[1.0, 1.0],
       [1.0, 1.0]]
      """, matrix.description)
    // Increase max scalar length.
    // Check that scalars are still aligned (properly left padded).
    matrix[0][1] = Tensor(-1)
    expectEqual("""
      [[ 1.0, -1.0],
       [ 1.0,  1.0]]
      """, matrix.description)
  }
}

StringDescriptionTests.test("HigherRankTensors") {
  do {
    let tensor = Tensor<Int32>(ones: [1, 1, 1, 1])
    expectEqual("[[[[1]]]]", tensor.description)
  }

  do {
    let tensor = Tensor<Int32>(ones: [3, 4, 5])
    expectEqual("""
      [[[1, 1, 1, 1, 1],
        [1, 1, 1, 1, 1],
        [1, 1, 1, 1, 1],
        [1, 1, 1, 1, 1]],

       [[1, 1, 1, 1, 1],
        [1, 1, 1, 1, 1],
        [1, 1, 1, 1, 1],
        [1, 1, 1, 1, 1]],

       [[1, 1, 1, 1, 1],
        [1, 1, 1, 1, 1],
        [1, 1, 1, 1, 1],
        [1, 1, 1, 1, 1]]]
      """, tensor.description)
  }

  // Test large tensor (above 1000 scalar threshold).
  do {
    let tensor = Tensor<Int32>(ones: [10, 10, 11])
    expectEqual("""
      [[[1, 1, 1, ..., 1, 1, 1],
        [1, 1, 1, ..., 1, 1, 1],
        [1, 1, 1, ..., 1, 1, 1],
        ...,
        [1, 1, 1, ..., 1, 1, 1],
        [1, 1, 1, ..., 1, 1, 1],
        [1, 1, 1, ..., 1, 1, 1]],

       [[1, 1, 1, ..., 1, 1, 1],
        [1, 1, 1, ..., 1, 1, 1],
        [1, 1, 1, ..., 1, 1, 1],
        ...,
        [1, 1, 1, ..., 1, 1, 1],
        [1, 1, 1, ..., 1, 1, 1],
        [1, 1, 1, ..., 1, 1, 1]],

       [[1, 1, 1, ..., 1, 1, 1],
        [1, 1, 1, ..., 1, 1, 1],
        [1, 1, 1, ..., 1, 1, 1],
        ...,
        [1, 1, 1, ..., 1, 1, 1],
        [1, 1, 1, ..., 1, 1, 1],
        [1, 1, 1, ..., 1, 1, 1]],

       ...,

       [[1, 1, 1, ..., 1, 1, 1],
        [1, 1, 1, ..., 1, 1, 1],
        [1, 1, 1, ..., 1, 1, 1],
        ...,
        [1, 1, 1, ..., 1, 1, 1],
        [1, 1, 1, ..., 1, 1, 1],
        [1, 1, 1, ..., 1, 1, 1]],

       [[1, 1, 1, ..., 1, 1, 1],
        [1, 1, 1, ..., 1, 1, 1],
        [1, 1, 1, ..., 1, 1, 1],
        ...,
        [1, 1, 1, ..., 1, 1, 1],
        [1, 1, 1, ..., 1, 1, 1],
        [1, 1, 1, ..., 1, 1, 1]],

       [[1, 1, 1, ..., 1, 1, 1],
        [1, 1, 1, ..., 1, 1, 1],
        [1, 1, 1, ..., 1, 1, 1],
        ...,
        [1, 1, 1, ..., 1, 1, 1],
        [1, 1, 1, ..., 1, 1, 1],
        [1, 1, 1, ..., 1, 1, 1]]]
      """, tensor.description)
  }
}

// Test example random initialization (longer scalars).
// Hard-coded test tensors generated via `Tensor(randomNormal:)`.
StringDescriptionTests.test("RandomScalars") {
  do {
    var matrix = Tensor<Float>(
      [[  1.1621541, -0.39326498],
       [ -1.5391855,    -1.11794]]
    )
    expectEqual("""
      [[  1.1621541, -0.39326498],
       [ -1.5391855,    -1.11794]]
      """, matrix.description)
    // Use one short scalar.
    // Check that scalars are still aligned (properly left padded).
    matrix[0][1] = Tensor(-1)
    expectEqual("""
      [[ 1.1621541,       -1.0],
       [-1.5391855,   -1.11794]]
      """, matrix.description)
  }

  do {
    var matrix = Tensor<Double>(
      [[  0.3374860907024739,   0.1805635511143933],
       [  0.1861132028235124, -0.08243178459215775]]
    )
    expectEqual("""
      [[  0.3374860907024739,   0.1805635511143933],
       [  0.1861132028235124, -0.08243178459215775]]
      """, matrix.description)
    // Use one short scalar.
    // Check that scalars are still aligned (properly left padded).
    matrix[0][1] = Tensor(-1)
    expectEqual("""
      [[  0.3374860907024739,                 -1.0],
       [  0.1861132028235124, -0.08243178459215775]]
      """, matrix.description)
  }
}

StringDescriptionTests.test("DescriptionConfiguration") {
  // Test `lineWidth` configuration.
  let vector = Tensor<Float>(ones: [10])
  expectEqual("[1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]",
              vector.description)
  expectEqual("""
    [1.0, 1.0,
     1.0, 1.0,
     1.0, 1.0,
     1.0, 1.0,
     1.0, 1.0]
    """, vector.description(lineWidth: 6))

  // Test `summarizing` configuration.
  // NOTE: `String.contains(_ substring: String)` requires Foundation.
#if canImport(Foundation)
  let longVector = Tensor<Float>(ones: [1001])
  expectTrue(longVector.description.contains("..."))
  expectFalse(longVector.description(summarizing: false).contains("..."))
#endif // canImport(Foundation)

  // Test `edgeElementCount` configuration.
  var tallMatrix = Tensor<Float>(ones: [50, 2])
  expectEqual("""
    [[1.0, 1.0],
     [1.0, 1.0],
     [1.0, 1.0],
     ...,
     [1.0, 1.0],
     [1.0, 1.0],
     [1.0, 1.0]]
    """,
    tallMatrix.description(summarizing: true))
  expectEqual("""
    [[1.0, 1.0],
     ...,
     [1.0, 1.0]]
    """,
    tallMatrix.description(edgeElementCount: 1, summarizing: true))
}

StringDescriptionTests.test("FullDescription") {
  let vector = Tensor<Float>(ones: [2, 2, 2])
  expectEqual("[[[1.0, 1.0], [1.0, 1.0]], [[1.0, 1.0], [1.0, 1.0]]]",
              vector.fullDescription)
}

runAllTests()
