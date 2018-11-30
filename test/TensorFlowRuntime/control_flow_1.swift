// RUN: %target-run-simple-swift %swift-tensorflow-test-run-extra-options
// RUN: %target-run-dynamic-compilation-swift %swift-tensorflow-test-run-extra-options
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize
//
// Compiler-only testing for TPU graph lowering (e.g. shape requirements by XLA).
// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -Xllvm -tf-dump-graph -Xllvm -tf-target-tpu -O -emit-sil %s >/dev/null

// Control flow related tests.

import TensorFlow
#if TPU
import TensorFlowUnittestTPU
#else
import TensorFlowUnittest
#endif
import StdlibUnittest

var ControlFlowTests = TestSuite("ControlFlow")

public enum Pet {
  case bird, cat, dog, fish
}

// Enumerated all cases.
@inline(never)
public func weighPet(_ pet: Pet,
                     _ expectedVal: Float) {
  var weight = Tensor<Float>(1.0)
  switch pet {
  case .bird:
    weight += 1.0
    // Currently we make this call and the ones below to declare a shape,
    // because when targeitng TPU, the bb arg for `weight` gets replicated onto
    // both CPU and TPU devices.
    // TODO: Remove these calls once we do bb arg pruning via dataflow analysis.
    weight = _scalarTensorWithShape(weight)
  case .cat:
    weight += 5.0
    weight = _scalarTensorWithShape(weight)
  case .dog:
    weight += 10.0
    weight = _scalarTensorWithShape(weight)
  case .fish: break // no tensor code here
  }
  expectNearlyEqualWithScalarTensor(expectedVal, weight)
  // TODO: remove the extra code below once TPU execution supports 0 output
  // tensors (b/111123797)
  let extra = Tensor<Float>(1.0)
  _hostOp(extra)
}
// TODO: fix the disabled GPU tests below.
#if !CUDA
ControlFlowTests.testAllBackends("weighPet") {
  weighPet(.bird, 2.0)
  weighPet(.cat, 6.0)
  weighPet(.dog, 11.0)
  weighPet(.fish, 1.0)
}
#endif // CUDA

@inline(never)
public func weighPetWithDefault(_ pet: Pet,
                                _ expectedVal: Float) {
  var weight = Tensor<Float>(1.0)
  switch pet {
  case .cat:
    weight += 5.0
    weight = _scalarTensorWithShape(weight)
  default:
    weight += 3.0
    weight = _scalarTensorWithShape(weight)
  }
  expectNearlyEqualWithScalarTensor(expectedVal, weight)
  // TODO: remove the extra code below once TPU execution supports 0 output
  // tensors (b/111123797)
  let extra = Tensor<Float>(1.0)
  _hostOp(extra)
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

@inline(never)
public func testEnumWithPayload(_ x: EnumWithPayload, _ expectedVal: Float) {
  var val = Tensor<Float>(2.0)
  switch x {
  case .a(let x):
    val += 1.0
    val = _scalarTensorWithShape(val)
    _hostOp(x)
  case .b(let x):
    _hostOp(x)
    let tx = Tensor<Float>(x).toAccelerator(shape: [])
    val += tx
    val = _scalarTensorWithShape(val)
  case .c(let x, let y):
    val *= x.toAccelerator(shape: []) + y.toAccelerator(shape: [])
    val = _scalarTensorWithShape(val)
    _hostOp(x)
    _hostOp(y)
  case .d(let f):
    val += 10.0
    val = _scalarTensorWithShape(val)
    _hostOp(f)
  }
  val += 0.0
  expectNearlyEqualWithScalarTensor(expectedVal, val)
}
#if !CUDA
ControlFlowTests.testAllBackends("testEnumWithPayload") {
  testEnumWithPayload(.a("Hello"), 3.0)
  testEnumWithPayload(.b(3.0), 5.0)
  testEnumWithPayload(.c(Tensor<Float>(1.0), Tensor<Float>(2.0)), 6.0)
  testEnumWithPayload(.d(.b(3.0)), 12.0)
}
#endif // CUDA

@inline(never)
public func testCondBranch(_ a: Bool,
                           _ expectedVal: Float) {
  var b = Tensor<Float>(2.0)
  if a {
    b += 1.0
  }
  b -= 1.0
  expectNearlyEqualWithScalarTensor(expectedVal, b)
}
ControlFlowTests.testAllBackends("testCondBranch") {
  testCondBranch(true, 2.0)
  testCondBranch(false, 1.0)
}


@inline(never)
public func testSwitchEnum(_ a: Tensor<Float>?,
                           _ expectedVal: Float) {
  var b = Tensor<Float>(2.0)
  if let a = a {
    b += a.toAccelerator(shape: [])
    b = _scalarTensorWithShape(b)
  }
  b -= 1.0
  expectNearlyEqualWithScalarTensor(expectedVal, b)
}
ControlFlowTests.testAllBackends("testSwitchEnum") {
  testSwitchEnum(Tensor<Float>(1.0), 2.0)
  testSwitchEnum(nil, 1.0)
}


public protocol P {}
public struct S: P {}
public enum EnumAddr {
  case A
  case B(P)
}
@inline(never)
public func testSwitchEnumAddr(_ a: EnumAddr,
                               _ expectedVal: Float) {
  var b = Tensor<Float>(2.0)
  switch a {
  case .A:
      b += 1.0
      b = _scalarTensorWithShape(b)
  default:
      break
  }
  b -= 1.0
  expectNearlyEqualWithScalarTensor(expectedVal, b)
}
ControlFlowTests.testAllBackends("testSwitchEnumAddr") {
  testSwitchEnumAddr(EnumAddr.A, 2.0)
  testSwitchEnumAddr(EnumAddr.B(S()), 1.0)
}


@inline(never)
public func testTryApply(_ a: Int,
                         _ expectedVal: Float) {
  enum MyError : Error {
    case E(String)
  }
  @inline(never)
  func foo(_ x: Int) throws {
    if x == 1 {
      throw MyError.E("err")
    }
  }

  var b = Tensor<Float>(2.0)
  do {
    try foo(a)
    b += 1.0
    b = _scalarTensorWithShape(b)
  } catch { }
  b -= 1.0
  expectNearlyEqualWithScalarTensor(expectedVal, b)
}
ControlFlowTests.testAllBackends("testTryApply") {
  testTryApply(0, 2.0)
  testTryApply(1, 1.0)
}


class X {}
class Y {}
public func testCheckedCastBranch(_ a: AnyObject,
                                  _ expectedVal: Float) {
  var b = Tensor<Float>(2.0)
  if a is X {
    b += 1.0
    b = _scalarTensorWithShape(b)
  }
  b -= 1.0
  expectNearlyEqualWithScalarTensor(expectedVal, b)
}
ControlFlowTests.testAllBackends("testCheckedCastBranch") {
  testCheckedCastBranch(X(), 2.0)
  testCheckedCastBranch(Y(), 1.0)
}


struct SS : P {}
@inline(never)
public func testCheckedCastAddrBranch(_ p: P,
                                      _ expectedVal: Float) {
  var b = Tensor<Float>(2.0)
  if let _ = p as? S {
    b += 1.0
    b = _scalarTensorWithShape(b)
  }
  b -= 1.0
  expectNearlyEqualWithScalarTensor(expectedVal, b)
}
ControlFlowTests.testAllBackends("testCheckedCastAddrBranch") {
  testCheckedCastAddrBranch(S(), 2.0)
  testCheckedCastAddrBranch(SS(), 1.0)
}

runAllTests()
