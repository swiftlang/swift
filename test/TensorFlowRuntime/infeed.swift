// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize
// TODO: retire this test when we turn on strict DA, and complete dataset/iterator support.
// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -Xllvm -tf-dump-graph -Xllvm -tf-target-tpu -O -emit-sil %s >/dev/null
//
// Infeed tests -- compiler-only tests for OSS.

import TensorFlow
#if TPU
import TensorFlowUnittestTPU
#else
import TensorFlowUnittest
#endif
import StdlibUnittest

var InfeedTests = TestSuite("Infeed")

// FIXME: Revisit how to enable infeed outside the context of dataset /
// iterators.
#if false
@inline(never)
func testScalarInput() {
  func add(_ x: Float, _ y: Float) -> Float {
    TensorFlow.enableTPU(infeed: false)
    let x = Tensor<Float>(x)
    let y = Tensor<Float>(y)
    return (x+y).array.scalars[0]
  }

  expectNearlyEqual(3.7, add(1.3, 2.4), byError: 0.1)
}
InfeedTests.testTPU("ScalarInput", testScalarInput)
#endif

InfeedTests.testTPU("JustDataset") {
  TensorFlow.enableTPU(infeed: true)

  let result = Tensor<Float>(handle: #tfop(
    "tfc.makeIteratorGetNextWithDatasets",
    dataSource: "fake",
    filePath: "dummy_path",
    batchSize: Int64(1),
    outputShapes: [TensorShape()]))
  // 1 is the magic output currently hard-coded.
  expectEqual(42.0, result.array.scalars[0])
}

InfeedTests.testTPU("DatasetWithOtherNodes") {
  TensorFlow.enableTPU(infeed: true)

  // 42.0 is the magic output of the iterator currently hard-coded.
  let x = Tensor<Float>(handle: #tfop(
    "tfc.makeIteratorGetNextWithDatasets",
    dataSource: "fake",
    filePath: "dummy_path",
    batchSize: Int64(1),
    outputShapes: [TensorShape()]))
  let result = x + 1
  expectEqual(43.0, result.array.scalars[0])
}

InfeedTests.testTPU("DatasetWithMnist") {
  TensorFlow.enableTPU(infeed: true)
  // FIXME: Replace the CNS directory with a dynamically generated file path
  // from this test's data dependency, so that the test will be hermetic.
  let (images1, labels1): (TensorHandle<Float>, TensorHandle<Int32>) = #tfop(
    "tfc.makeIteratorGetNextWithDatasets",
    dataSource: "mnist",
    filePath: "/cns/ok-d/home/sasabour/mnist",
    batchSize: Int64(2048),
    output_shapes: [TensorShape(2048,784), TensorShape(2048)])
  let images = Tensor<Float>(handle: #tfop("Identity", images1))
  let labels = Tensor<Int32>(handle: #tfop("Identity", labels1))
  // Add some more graph nodes consuming the output of the iterator.
  let imagesMod = images + 1
  let labelsMod = labels + 2
  expectEqual([2048,784], imagesMod.array.shape)
  expectEqual([2048], labelsMod.array.shape)
}

#if false
// This test runs on cloud TPU, but not on Forge yet due to the dynamic path
// challenge described below.
InfeedTests.testTPU("DatasetWithImagenet") {
  TensorFlow.enableTPU(infeed: true)
  // FIXME: We need to set a dynamic file path (based on the scheduled Forge
  // machine) for reading the TFRecord data at runtime, but the TF graph
  // (storing a string attribute of that file path) is generated at compile
  // time.
  //
  // One option is to rewrite the graph at runtime to set that string attribute,
  // before calling TF_SessionRun().
  let (images1, labels1): (TensorHandle<Float>, TensorHandle<Int32>) = #tfop(
    "tfc.makeIteratorGetNextWithDatasets",
    dataSource: "imagenet",
    filePath: "gs://cloudtpu-imagenet-data/train/train-*",
    batchSize: 64,
    output_shapes: [TensorShape(64,224,224,3), TensorShape(64)])
  let images = Tensor<Float>(handle: #tfop("Identity", images1))
  let labels = Tensor<Int32>(handle: #tfop("Identity", labels1))
  // Add some more graph nodes consuming the output of the iterator.
  let imagesMod = images + 1
  let labelsMod = labels + 2
  expectEqual([64, 224, 224, 3], imagesMod.array.shape)
  expectEqual([64], labelsMod.array.shape)
}
#endif

#if false
// TODO(hongm): Extend shape info support to make this test work.
// TODO(hongm): Unify with TensorTests.ElementIndexing
InfeedTests.testTPU("ElementIndexing") {
  TensorFlow.enableTPU(infeed: false)

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
