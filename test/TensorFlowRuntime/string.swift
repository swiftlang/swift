// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize
//
// String Tensor tests.

import TensorFlow
import StdlibUnittest

var StringTensorTests = TestSuite("String")

StringTensorTests.test("StringComparison") {
  let t1: StringTensorHandle = #tfop(
    "Const",
    dtype: String.self,
    value$tensor: "foo"
  )
  let result1 = Tensor<Bool>(handle: #tfop("Equal", t1, t1))
  expectEqual(ShapedArray(shape: [], scalars: [true]), result1.array)

  let t2: StringTensorHandle = #tfop(
    "Const",
    dtype: String.self,
    value$tensor: ["foo", "bar"],
    value$shape: TensorShape(2)
  )
  let result2 = Tensor<Bool>(handle: #tfop("Equal", t2, t2))
  expectEqual(ShapedArray(shape: [2], scalars: [true, true]),
              result2.array)
}

runAllTests()
