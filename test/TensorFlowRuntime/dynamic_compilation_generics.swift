// RUN: %target-run-dynamic-compilation-swift
// REQUIRES: executable_test

import TensorFlow
import TensorFlowUnittest
import StdlibUnittest

var DynamicCompilationGenericsTest = TestSuite("DynamicCompilationGenerics")

@inline(never)
func tensorSliceDataset<T: TensorGroup>(_ t: T) -> VariantHandle {
  return #tfop("TensorSliceDataset", t, Toutput_types$dtype: T._typeList,
               output_shapes: T._unknownShapeList)
}

@inline(never)
func first<T: TensorGroup>(_ dataset: VariantHandle) -> T {
  let iterator: ResourceHandle = #tfop(
    "IteratorV2", shared_name: "blah", container: "earth",
    output_types$dtype: T._typeList, output_shapes: T._unknownShapeList)
  #tfop("MakeIterator", dataset, iterator) as Void
  return #tfop("IteratorGetNext", iterator, output_types$dtype: T._typeList,
               output_shapes: T._unknownShapeList)
}

struct Example {
  let x, y: Tensor<Float>
}

extension Example : TensorGroup {
  static let _tensorHandleCount: Int32 = 2
  static let _unknownShapeList: [TensorShape?] = [nil, nil]
  static let _typeList: [TensorDataType] =
      [Float.tensorFlowDataType, Float.tensorFlowDataType]
  func _unpackTensorHandles(resultBuffer: UnsafeMutablePointer<CTensorHandle>) {
    resultBuffer.advanced(by: 0).initialize(to: x.handle.cTensorHandle)
    resultBuffer.advanced(by: 1).initialize(to: y.handle.cTensorHandle)
  }
  init(_owning tensorHandles: UnsafePointer<CTensorHandle>) {
    x = Tensor(handle: TensorHandle(owning: tensorHandles.advanced(by: 0).pointee))
    y = Tensor(handle: TensorHandle(owning: tensorHandles.advanced(by: 1).pointee))
  }
}

DynamicCompilationGenericsTest.testAllBackends("dataset") {
  let dataset = tensorSliceDataset(Example(x: Tensor([1, 2, 3]), y: Tensor([2, 4, 6])))
  let example: Example = first(dataset)
  expectEqual(1, example.x.scalar!)
  expectEqual(2, example.y.scalar!)
}

runAllTests()
