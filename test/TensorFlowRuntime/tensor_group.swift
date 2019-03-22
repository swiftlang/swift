// RUN: %empty-directory(%t)
// RUN: %target-build-swift -swift-version 5 -g %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out
// REQUIRES: executable_test
//
// `TensorGroup` tests.

import TensorFlow
import StdlibUnittest

var TensorGroupTests = TestSuite("TensorGroup")

extension TensorDataType : Equatable {
  public static func == (lhs: TensorDataType, rhs: TensorDataType) -> Bool {
    return Int(lhs._cDataType.rawValue) == Int(rhs._cDataType.rawValue)
  }
}

struct Empty : TensorGroup {}

struct Simple : TensorGroup {
  var w, b: Tensor<Float>
}

struct Mixed : TensorGroup {
  // Mutable.
  var string: StringTensor
  var float: Tensor<Float>
  // Immutable.
  let int: Tensor<Int32>
}

struct Nested : TensorGroup {
  // Immutable.
  let simple: Simple
  // Mutable.
  var mixed: Mixed
}

struct Generic<T: TensorGroup, U: TensorGroup> : TensorGroup {
  var t: T
  var u: U
}

struct NestedGeneric<T: TensorGroup> {
  struct Nested<U: TensorGroup> {
    struct UltraNested<V: TensorGroup> : TensorGroup {
      var a: Generic<T, U>
      var b: Generic<U, V>
      var c: Generic<T, V>
    }
  }
}

TensorGroupTests.test("Empty") {
  expectEqual([], Empty._typeList)
}

TensorGroupTests.test("Simple") {
  let float = Float.tensorFlowDataType
  expectEqual([float, float], Simple._typeList)
}

TensorGroupTests.test("Mixed") {
  let float = Float.tensorFlowDataType
  let int = Int32.tensorFlowDataType
  let string = String.tensorFlowDataType
  expectEqual([string, float, int], Mixed._typeList)
}

TensorGroupTests.test("Nested") {
  let float = Float.tensorFlowDataType
  let int = Int32.tensorFlowDataType
  let string = String.tensorFlowDataType
  expectEqual([float, float, string, float, int], Nested._typeList)
}

TensorGroupTests.test("Generic") {
  let float = Float.tensorFlowDataType
  let int = Int32.tensorFlowDataType
  let string = String.tensorFlowDataType
  expectEqual(
    [float, float, string, float, int], 
    Generic<Simple, Mixed>._typeList)
}

TensorGroupTests.test("NestedGeneric") {
  let float = Float.tensorFlowDataType
  let int = Int32.tensorFlowDataType
  let string = String.tensorFlowDataType
  expectEqual(
    [float, float, string, float, int, 
     string, float, int, float, 
     float, float, float],
    NestedGeneric<Simple>.Nested<Mixed>.UltraNested<Tensor<Float>>._typeList)
}

runAllTests()
