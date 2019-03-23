// RUN: %empty-directory(%t)
// RUN: %target-build-swift -swift-version 5 -g %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out
// REQUIRES: executable_test
//
// `TensorGroup` tests.

import CTensorFlow
import TensorFlow
import StdlibUnittest

var TensorGroupTests = TestSuite("TensorGroup")

extension TensorDataType : Equatable {
  public static func == (lhs: TensorDataType, rhs: TensorDataType) -> Bool {
    return Int(lhs._cDataType.rawValue) == Int(rhs._cDataType.rawValue)
  }
}

struct Empty : TensorGroup {}

struct Simple : TensorGroup, Equatable {
  var w, b: Tensor<Float>
}

struct Mixed : TensorGroup, Equatable {
  // Mutable.
  var float: Tensor<Float>
  // Immutable.
  let int: Tensor<Int32>
}

struct Nested : TensorGroup, Equatable {
  // Immutable.
  let simple: Simple
  // Mutable.
  var mixed: Mixed
}

struct Generic<T: TensorGroup & Equatable, U: TensorGroup & Equatable> : TensorGroup, Equatable {
  var t: T
  var u: U
}

struct NestedGeneric<T: TensorGroup & Equatable> {
  struct Nested<U: TensorGroup & Equatable> {
    struct UltraNested<V: TensorGroup & Equatable> : TensorGroup, Equatable {
      var a: Generic<T, U>
      var b: Generic<U, V>
      var c: Generic<T, V>
    }
  }
}

TensorGroupTests.test("EmptyTypeList") {
  expectEqual([], Empty._typeList)
}

TensorGroupTests.test("SimpleTypeList") {
  let float = Float.tensorFlowDataType
  expectEqual([float, float], Simple._typeList)
}

TensorGroupTests.test("SimpleInit") {
  let w = Tensor<Float>(0.1)
  let b = Tensor<Float>(0.1)
  let simple = Simple(w: w, b: b)

  let status = TF_NewStatus()
  let wHandle = TFE_TensorHandleCopySharingTensor(w.handle._cTensorHandle, status)!
  let bHandle = TFE_TensorHandleCopySharingTensor(b.handle._cTensorHandle, status)!
  TF_DeleteStatus(status)

  let buffer = UnsafeMutableBufferPointer<CTensorHandle>.allocate(capacity: 2)
  buffer.baseAddress?.initialize(to: wHandle)
  buffer.baseAddress?.advanced(by: 1).initialize(to: bHandle)
  let expectedSimple = Simple(_owning: UnsafePointer(buffer.baseAddress))

  expectEqual(expectedSimple, simple)
}

TensorGroupTests.test("MixedTypeList") {
  let float = Float.tensorFlowDataType
  let int = Int32.tensorFlowDataType
  expectEqual([float, int], Mixed._typeList)
}

TensorGroupTests.test("MixedInit") {
  let float = Tensor<Float>(0.1)
  let int = Tensor<Int32>(1)
  let mixed = Mixed(float: float, int: int)

  let status = TF_NewStatus()
  let floatHandle = TFE_TensorHandleCopySharingTensor(float.handle._cTensorHandle, status)!
  let intHandle = TFE_TensorHandleCopySharingTensor(int.handle._cTensorHandle, status)!
  TF_DeleteStatus(status)

  let buffer = UnsafeMutableBufferPointer<CTensorHandle>.allocate(capacity: 2)
  buffer.baseAddress?.initialize(to: floatHandle)
  buffer.baseAddress?.advanced(by: 1).initialize(to: intHandle)
  let expectedMixed = Mixed(_owning: UnsafePointer(buffer.baseAddress))

  expectEqual(expectedMixed, mixed)
}

TensorGroupTests.test("NestedTypeList") {
  let float = Float.tensorFlowDataType
  let int = Int32.tensorFlowDataType
  expectEqual([float, float, float, int], Nested._typeList)
}

TensorGroupTests.test("NestedInit") {
  let w = Tensor<Float>(0.1)
  let b = Tensor<Float>(0.1)
  let simple = Simple(w: w, b: b)
  let float = Tensor<Float>(0.1)
  let int = Tensor<Int32>(1)
  let mixed = Mixed(float: float, int: int)
  let nested = Nested(simple: simple, mixed: mixed)

  let status = TF_NewStatus()
  let wHandle = TFE_TensorHandleCopySharingTensor(w.handle._cTensorHandle, status)!
  let bHandle = TFE_TensorHandleCopySharingTensor(b.handle._cTensorHandle, status)!
  let floatHandle = TFE_TensorHandleCopySharingTensor(float.handle._cTensorHandle, status)!
  let intHandle = TFE_TensorHandleCopySharingTensor(int.handle._cTensorHandle, status)!
  TF_DeleteStatus(status)

  let buffer = UnsafeMutableBufferPointer<CTensorHandle>.allocate(capacity: 4)
  buffer.baseAddress?.initialize(to: wHandle)
  buffer.baseAddress?.advanced(by: 1).initialize(to: bHandle)
  buffer.baseAddress?.advanced(by: 2).initialize(to: floatHandle)
  buffer.baseAddress?.advanced(by: 3).initialize(to: intHandle)
  let expectedNested = Nested(_owning: UnsafePointer(buffer.baseAddress))

  expectEqual(expectedNested, nested)
}

TensorGroupTests.test("GenericTypeList") {
  let float = Float.tensorFlowDataType
  let int = Int32.tensorFlowDataType
  expectEqual(
    [float, float, float, int], 
    Generic<Simple, Mixed>._typeList)
}

TensorGroupTests.test("GenericInit") {
  let w = Tensor<Float>(0.1)
  let b = Tensor<Float>(0.1)
  let simple = Simple(w: w, b: b)
  let float = Tensor<Float>(0.1)
  let int = Tensor<Int32>(1)
  let mixed = Mixed(float: float, int: int)
  let generic = Generic(t: simple, u: mixed)

  let status = TF_NewStatus()
  let wHandle = TFE_TensorHandleCopySharingTensor(w.handle._cTensorHandle, status)!
  let bHandle = TFE_TensorHandleCopySharingTensor(b.handle._cTensorHandle, status)!
  let floatHandle = TFE_TensorHandleCopySharingTensor(float.handle._cTensorHandle, status)!
  let intHandle = TFE_TensorHandleCopySharingTensor(int.handle._cTensorHandle, status)!
  TF_DeleteStatus(status)

  let buffer = UnsafeMutableBufferPointer<CTensorHandle>.allocate(capacity: 4)
  buffer.baseAddress?.initialize(to: wHandle)
  buffer.baseAddress?.advanced(by: 1).initialize(to: bHandle)
  buffer.baseAddress?.advanced(by: 2).initialize(to: floatHandle)
  buffer.baseAddress?.advanced(by: 3).initialize(to: intHandle)
  let expectedGeneric = Generic<Simple, Mixed>(_owning: UnsafePointer(buffer.baseAddress))

  expectEqual(expectedGeneric, generic)
}

TensorGroupTests.test("NestedGenericTypeList") {
  let float = Float.tensorFlowDataType
  let int = Int32.tensorFlowDataType
  expectEqual(
    [float, float, float, int, 
     float, int, float, 
     float, float, float],
    NestedGeneric<Simple>.Nested<Mixed>.UltraNested<Tensor<Float>>._typeList)
}

TensorGroupTests.test("NestedGenericInit") {
  let w = Tensor<Float>(0.1)
  let b = Tensor<Float>(0.1)
  let simple = Simple(w: w, b: b)
  let float = Tensor<Float>(0.1)
  let int = Tensor<Int32>(1)
  let mixed = Mixed(float: float, int: int)
  let genericSM = Generic<Simple, Mixed>(t: simple, u: mixed)
  let genericMF = Generic<Mixed, Tensor<Float>>(t: mixed, u: w)
  let genericSF = Generic<Simple, Tensor<Float>>(t: simple, u: w)
  let generic = NestedGeneric.Nested.UltraNested(a: genericSM, b: genericMF, c: genericSF)

  let status = TF_NewStatus()
  let wHandle1 = TFE_TensorHandleCopySharingTensor(w.handle._cTensorHandle, status)!
  let wHandle2 = TFE_TensorHandleCopySharingTensor(w.handle._cTensorHandle, status)!
  let wHandle3 = TFE_TensorHandleCopySharingTensor(w.handle._cTensorHandle, status)!
  let wHandle4 = TFE_TensorHandleCopySharingTensor(w.handle._cTensorHandle, status)!
  let bHandle1 = TFE_TensorHandleCopySharingTensor(b.handle._cTensorHandle, status)!
  let bHandle2 = TFE_TensorHandleCopySharingTensor(b.handle._cTensorHandle, status)!
  let floatHandle1 = TFE_TensorHandleCopySharingTensor(float.handle._cTensorHandle, status)!
  let floatHandle2 = TFE_TensorHandleCopySharingTensor(float.handle._cTensorHandle, status)!
  let intHandle1 = TFE_TensorHandleCopySharingTensor(int.handle._cTensorHandle, status)!
  let intHandle2 = TFE_TensorHandleCopySharingTensor(int.handle._cTensorHandle, status)!
  TF_DeleteStatus(status)

  let buffer = UnsafeMutableBufferPointer<CTensorHandle>.allocate(capacity: 10)
  buffer.baseAddress?.initialize(to: wHandle1)
  buffer.baseAddress?.advanced(by: 1).initialize(to: bHandle1)
  buffer.baseAddress?.advanced(by: 2).initialize(to: floatHandle1)
  buffer.baseAddress?.advanced(by: 3).initialize(to: intHandle1)
  buffer.baseAddress?.advanced(by: 4).initialize(to: floatHandle2)
  buffer.baseAddress?.advanced(by: 5).initialize(to: intHandle2)
  buffer.baseAddress?.advanced(by: 6).initialize(to: wHandle2)
  buffer.baseAddress?.advanced(by: 7).initialize(to: wHandle3)
  buffer.baseAddress?.advanced(by: 8).initialize(to: bHandle2)
  buffer.baseAddress?.advanced(by: 9).initialize(to: wHandle4)
  let expectedGeneric = NestedGeneric<Simple>.Nested<Mixed>.UltraNested<Tensor<Float>>(
    _owning: UnsafePointer(buffer.baseAddress))

  expectEqual(expectedGeneric, generic)
}

runAllTests()
