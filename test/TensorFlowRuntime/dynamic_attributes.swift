// RUN: %target-run-dynamic-compilation-swift --attributes-json %S/Inputs/attributes.json
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize
// REQUIRES: tensorflow

import TensorFlow
import TensorFlowUnittest
import StdlibUnittest

var DynamicAttributeTests = TestSuite("DynamicAttribute")

// These global vars with @inline(never) loaders ensure that the compiler
// can't find the constant value of the attributes.

var dtypeInt32 = Int32.tensorFlowDataType
@inline(never)
func loadDtypeInt32() -> TensorDataType {
  return dtypeInt32
}

var dtypeDouble = Double.tensorFlowDataType
@inline(never)
func loadDtypeDouble() -> TensorDataType {
  return dtypeDouble
}

DynamicAttributeTests.test("single TFDataTypeAttribute") {
  let t1 = Tensor<Int32>(-1)
  let t1Result: Tensor<Int32> = #tfop("Abs", t1, T$dtype: loadDtypeInt32())
  expectEqual(1, t1Result.scalar!)

  let t2 = Tensor<Double>(-2)
  let t2Result: Tensor<Double> = #tfop("Abs", t2, T$dtype: loadDtypeDouble())
  expectEqual(2, t2Result.scalar!)
}

runAllTests()
