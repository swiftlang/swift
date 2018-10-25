// RUN: %target-run-dynamic-compilation-swift
// REQUIRES: executable_test

import TensorFlow
import TensorFlowUnittest
import StdlibUnittest

var DynamicCompilationGenericsTest = TestSuite("DynamicCompilationGenerics")

/// This function cannot be compiled in graph mode because it passes a value
/// of generic type T to a #tfop.
@inline(never)
func tensorSliceDataset<T: TensorGroup>(_ t: T) -> VariantHandle {
  return #tfop("TensorSliceDataset", [t], Toutput_types$dtype: T._typeList,
               output_shapes: T._unknownShapeList)
}

/// This function cannot be compiled in graph mode because it executes a #tfop
/// with generic return type T.
@inline(never)
func first<T: TensorGroup>(_ dataset: VariantHandle) -> T {
  let iterator: ResourceHandle = #tfop(
    "IteratorV2", shared_name: "blah", container: "earth",
    output_types$dtype: T._typeList, output_shapes: T._unknownShapeList)
  #tfop("MakeIterator", dataset, iterator) as Void
  return #tfop("IteratorGetNext", iterator, output_types$dtype: T._typeList,
               output_shapes: T._unknownShapeList)
}

/// This function cannot be compiled in graph mode because it executes a #tfop
/// with generic return type T.
@inline(never)
func unpack<T: TensorGroup, Scalar: AccelerableByTensorFlow>(
    _ tensor: Tensor<Scalar>
) -> T {
  return #tfop("Unpack", tensor, num: Int64(T._tensorHandleCount),
               T$dtype: Scalar.tensorFlowDataType)
}

struct Example {
  let x, y: Tensor<Float>
}

extension Example : TensorGroup {
  static let _typeList: [TensorDataType] =
      [Float.tensorFlowDataType, Float.tensorFlowDataType]
  func _unpackTensorHandles(into address: UnsafeMutablePointer<CTensorHandle>?) {
   address!.advanced(by: 0).initialize(to: x.handle._cTensorHandle)
   address!.advanced(by: 1).initialize(to: y.handle._cTensorHandle)
  }
  init(_owning tensorHandles: UnsafePointer<CTensorHandle>?) {
    x = Tensor(handle: TensorHandle(_owning: tensorHandles!.advanced(by: 0).pointee))
    y = Tensor(handle: TensorHandle(_owning: tensorHandles!.advanced(by: 1).pointee))
  }
}

struct EmptyExample : Equatable {}

extension EmptyExample : TensorGroup {
  static let _typeList: [TensorDataType] = []
  func _unpackTensorHandles(into address: UnsafeMutablePointer<CTensorHandle>?) {}
  init(_owning tensorHandles: UnsafePointer<CTensorHandle>?) {}
}

DynamicCompilationGenericsTest.testAllBackends("dataset") {
  let dataset = tensorSliceDataset(Example(x: Tensor([1, 2, 3]), y: Tensor([4, 5, 6])))
  let example: Example = first(dataset)
  expectEqual(1, example.x.scalar!)
  expectEqual(4, example.y.scalar!)
}

DynamicCompilationGenericsTest.testAllBackends("unpack") {
  let tensor = Tensor<Float>([[1, 2], [3, 4]])
  let example: Example = unpack(tensor)
  expectEqual(ShapedArray([1, 2]), example.x.array)
  expectEqual(ShapedArray([3, 4]), example.y.array)
}

DynamicCompilationGenericsTest.testAllBackends("unpack to empty struct") {
  let tensor = Tensor<Float>(shape: [0], scalars: [])
  let example: EmptyExample = unpack(tensor)
  expectEqual(EmptyExample(), example)
}

runAllTests()
