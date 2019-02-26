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

struct Simple : TensorGroup {
  var w, b: Tensor<Float>
}

struct Mixed : TensorGroup {
  // Mutable.
  var string: StringTensor
  var float: Tensor<Float>
  // Immutable.
  let int: Tensor<Int>
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

TensorGroupTests.test("Simple") {
  var x = Simple(w: 1, b: 2)
  let float = Float.tensorFlowDataType
  expectEqual([float, float], x._typeList)
}

KeyPathIterableTests.test("Mixed") {
  var x = Mixed(
    string: StringTensor("hello"), float: Tensor<Float>(.pi), 
    int: Tensor<Int>(0))
  let float = Float.tensorFlowDataType
  let int = Int.tensorFlowDataType
  let string = String.tensorFlowDataType
  expectEqual([string, float, int], x._typeList)
}

KeyPathIterableTests.test("Nested") {
  var s = Simple(w: 1, b: 2)
  var m = Mixed(
    string: StringTensor("hello"), float: Tensor<Float>(.pi), 
    int: Tensor<Int>(0))
  var x = Nested(simple: s, mixed: m)
  let float = Float.tensorFlowDataType
  let int = Int.tensorFlowDataType
  let string = String.tensorFlowDataType
  expectEqual([float, float, string, float, int], x._typeList)
}

KeyPathIterableTests.test("Generic") {
  var s = Simple(w: 1, b: 2)
  var m = Mixed(
    string: StringTensor("hello"), float: Tensor<Float>(.pi), 
    int: Tensor<Int>(0))
  var x = Generic(t: s, u: m)
  let float = Float.tensorFlowDataType
  let int = Int.tensorFlowDataType
  let string = String.tensorFlowDataType
  expectEqual([float, float, string, float, int], x._typeList)
}
