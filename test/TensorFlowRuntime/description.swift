// RUN: %target-run-eager-swift %swift-tensorflow-test-run-extra-options
// RUN: %target-run-gpe-swift %swift-tensorflow-test-run-extra-options
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize
//
// Tensor description (`CustomStringConvertible` conformance) tests.

import TensorFlow
import StdlibUnittest

var DescriptionTests = TestSuite("DescriptionTests")

DescriptionTests.test("Empty") {
  let empty = Tensor<Float>([] as [Float])
  expectEqual("[]", empty.description)
}

DescriptionTests.test("Scalar") {
  do {
    let scalar = Tensor<Int32>(1)
    expectEqual("1", scalar.description)
  }

  do {
    let scalar = Tensor<Float>(-3.14)
    expectEqual("-3.14", scalar.description)
  }
}

DescriptionTests.test("Vector") {
  do {
    let vector = Tensor<Int32>(ones: [4])
    expectEqual("[1, 1, 1, 1]", vector.description)
  }

  do {
    var vector = Tensor<Float>([1, 2, 3, 4])
    expectEqual("[1.0, 2.0, 3.0, 4.0]", vector.description)
    vector[1] = Tensor<Float>(-2)
    // vector = Tensor<Float>([1, -2, 3, 4])
    expectEqual("[ 1.0, -2.0,  3.0,  4.0]", vector.description)
  }

  // Test long vector (above 1000 scalar threshold).
  do {
    let vector = Tensor<Float>(repeating: 3, shape: [1001])
    expectEqual("[3.0, 3.0, 3.0, ..., 3.0, 3.0, 3.0]", vector.description)
  }
}

DescriptionTests.test("Matrix") {
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

DescriptionTests.test("HigherRankTensors") {
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
DescriptionTests.test("RandomScalars") {
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

DescriptionTests.test("DescriptionConfiguration") {
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

  // Test `summarize` configuration.
  let longVector = Tensor<Float>(ones: [1001])
  expectTrue(longVector.description.contains("..."))
  expectFalse(longVector.description(summarize: false).contains("..."))

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
    tallMatrix.description(summarize: true))
  expectEqual("""
    [[1.0, 1.0],
     ...,
     [1.0, 1.0]]
    """,
    tallMatrix.description(edgeElementCount: 1, summarize: true))
}

DescriptionTests.test("FullDescription") {
  let vector = Tensor<Float>(ones: [2, 2, 2])
  expectEqual("[[[1.0, 1.0], [1.0, 1.0]], [[1.0, 1.0], [1.0, 1.0]]]",
              vector.fullDescription)
}

runAllTests()
