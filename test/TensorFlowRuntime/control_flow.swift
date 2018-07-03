// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize

// Control flow related tests.

import TensorFlow
import TensorFlowUnittest
import StdlibUnittest

var ControlFlowTests = TestSuite("ControlFlow")

@inline(never)
public  func testSwitchEnum(_ a: Tensor<Float>?,
                            _ expectedVal: Float) {
  var b = Tensor<Float>(2.0)
  if let a = a {
    b += a
  }
  b -= 1.0
  expectNearlyEqualWithScalarTensor(expectedVal, b)
}
ControlFlowTests.testAllBackends("testSwitchEnum") {
  testSwitchEnum(Tensor<Float>(1.0), 2.0)
  testSwitchEnum(nil, 1.0)
}


public enum Pet {
  case bird, cat, dog, fish
}

// Enumerated all cases.
public func weighPet(_ pet: Pet,
                     _ expectedVal: Float) {
  var weight = Tensor<Float>(1.0)
  switch pet {
  case .bird: weight += 1.0
  case .cat: weight += 5.0
  case .dog: weight += 10.0
  case .fish: break // no tensor code here
  }
  // This is needed to work-around the current TF limitation where the `If` op
  // must produce some output tensors.
  // FIXME: lift this restriction.
  weight += 0.0
  expectNearlyEqualWithScalarTensor(expectedVal, weight)
}
ControlFlowTests.testAllBackends("weighPet") {
  weighPet(.bird, 2.0)
  weighPet(.cat, 6.0)
  weighPet(.dog, 11.0)
  weighPet(.fish, 1.0)
}

public func weighPetWithDefault(_ pet: Pet,
                                _ expectedVal: Float) {
  var weight = Tensor<Float>(1.0)
  switch pet {
  case .cat: weight += 5.0
  default: weight += 3.0
  }
  // This is needed to work-around the current TF limitation where the `If` op
  // must produce some output tensors.
  // FIXME: lift this restriction.
  weight += 0.0
  expectNearlyEqualWithScalarTensor(expectedVal, weight)
}
ControlFlowTests.testAllBackends("weighPetWithDefault") {
  weighPetWithDefault(.cat, 6.0)
  weighPetWithDefault(.bird, 4.0)
  weighPetWithDefault(.dog, 4.0)
  weighPetWithDefault(.fish, 4.0)
}


public enum EnumWithPayload {
  case a(String)
  case b(Float)
  case c(Tensor<Float>, Tensor<Float>)
  indirect case d(EnumWithPayload)
}

public func testEnumWithPayload(_ x: EnumWithPayload) -> Tensor<Float> {
  var val = Tensor<Float>(2.0)
  switch x {
  case .a(let x):
    val += 1.0
    print(x)
  case .b(let x):
    print(x)
    val += Tensor<Float>(x)
  case .c(let x, let y):
    val *= x + y
    print(x, y)
  case .d(let f):
    val += 10.0
    print(f)
  }
  val += 0.0
  return val
}
ControlFlowTests.testAllBackends("testEnumWithPayload") {
  expectNearlyEqualWithScalarTensor(3.0, testEnumWithPayload(.a("Hello")))
  expectNearlyEqualWithScalarTensor(5.0, testEnumWithPayload(.b(3.0)))
  expectNearlyEqualWithScalarTensor(6.0, testEnumWithPayload(
                                           .c(Tensor<Float>(1.0), Tensor<Float>(2.0))))
  expectNearlyEqualWithScalarTensor(12.0, testEnumWithPayload(.d(.b(3.0))))
}

runAllTests()
