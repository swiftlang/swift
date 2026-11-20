// REQUIRES: executable_test
// RUN: %target-run-simple-swift
// RUN: %target-swift-frontend -emit-sil -O -sil-verify-all -module-name adcs_capture_shared %s -o %t.sil
// RUN: %FileCheck %s < %t.sil

// Two predecessor payloads share one indirect capture allocation. Cleanup
// must deallocate it once.
import _Differentiation
import DifferentiationUnittest
import StdlibUnittest

@differentiable(reverse, wrt: (x, y))
public func labeled<T>(_ x: Float, _ y: Float, _ value: T) -> Float {
  2 * x + 3 * y
}

@derivative(of: labeled, wrt: (x, y))
@inline(__always)
public func vjpLabeled<T>(
  _ x: Float, _ y: Float, _ value: T
) -> (value: Float, pullback: (Float) -> (Float, Float)) {
  (2 * x + 3 * y, { seed in withExtendedLifetime(value) { (2 * seed, 3 * seed) } })
}

@differentiable(reverse, wrt: x)
public func stringShared(_ x: Float, _ name: String, _ flag: Bool) -> Float {
  let y = labeled(x, Float(1), name)
  let z = x * x
  if flag { return y * z }
  return y + z
}

// Differentiating both inputs avoids the subset thunk and exercises the
// indirect String capture in a direct pullback call.
@differentiable(reverse, wrt: (x, y))
public func stringDirect(_ x: Float, _ y: Float, _ name: String, _ flag: Bool) -> Float {
  if flag { return labeled(x, y, name) }
  return x * y
}

// CHECK-LABEL: {{^}}// reverse-mode derivative of stringShared(_:_:_:)
// CHECK:       bb0(%[[#X:]] : $Float, %[[#NAME:]] : $String,
// CHECK-NOT:     alloc_stack $String
// CHECK:         %[[#NAME_CAPTURE:]] = tuple (%[[#NAME]])
// CHECK-NEXT:    %[[#FLOAT_CAPTURES:]] = tuple (%[[#X]], %[[#X]])
// CHECK-NEXT:    %[[#SHARED:]] = tuple (%[[#NAME_CAPTURE]], %[[#FLOAT_CAPTURES]])
// CHECK:         %[[#]] = enum $_AD__{{[^, ]+}}_bb1__Pred__{{[^, ]+}}_spec{{[^, ]+}}, {{.*}}%[[#SHARED]]
// CHECK:         %[[#]] = enum $_AD__{{[^, ]+}}_bb2__Pred__{{[^, ]+}}_spec{{[^, ]+}}, {{.*}}%[[#SHARED]]
// CHECK:         // function_ref specialized pullback of stringShared(_:_:_:)
// CHECK-NEXT:    %[[#PB:]] = function_ref
// CHECK-NEXT:    %[[#]] = partial_apply [callee_guaranteed] %[[#PB]]
// CHECK:       } // end sil function

// CHECK-LABEL: {{^}}// specialized pullback of stringShared(_:_:_:)
// CHECK:       bb[[#]](%[[#]] : $Builtin.FPIEEE32, %[[#]] : $Builtin.FPIEEE32, %[[#JOIN:]] : $((_: String), (Float, Float)))
// CHECK-NEXT:    %[[#CAPTURES:]] = tuple_extract %[[#JOIN]], 0
// CHECK:         %[[#]] = tuple_extract %[[#CAPTURES]], 0
// CHECK-NOT:     apply {{.*}} : $@callee_guaranteed
// CHECK:       } // end sil function

// CHECK-LABEL: {{^}}// specialized pullback of stringDirect(_:_:_:_:)
// CHECK:       bb[[#]](%[[#PAYLOAD:]] : $(predecessor: {{[^,]+}}, (_: String)))
// CHECK-NEXT:    %[[#DIRECT_CAPTURES:]] = tuple_extract %[[#PAYLOAD]], 1
// CHECK-NEXT:    %[[#]] = tuple_extract %[[#DIRECT_CAPTURES]], 0
// CHECK-NOT:     apply {{.*}} : $@callee_guaranteed
// CHECK:       } // end sil function

var tests = TestSuite("ADCSSharedCapture")
tests.testWithLeakChecking("SharedIndirectString") {
  let name = String(repeating: "captured label", count: 7)
  let product = valueWithPullback(at: Float(3), of: {
    stringShared($0, name, true)
  })
  expectEqual(81, product.value)
  expectEqual(360, product.pullback(5))
  expectEqual(144, product.pullback(2))

  let sum = valueWithPullback(at: Float(3), of: {
    stringShared($0, name, false)
  })
  expectEqual(18, sum.value)
  expectEqual(40, sum.pullback(5))
}

tests.testWithLeakChecking("DirectIndirectString") {
  let name = String(repeating: "captured label", count: 7)
  let linear = valueWithPullback(at: Float(3), Float(4), of: {
    stringDirect($0, $1, name, true)
  })
  expectEqual(18, linear.value)
  expectEqual((10, 15), linear.pullback(5))
  expectEqual((4, 6), linear.pullback(2))

  let product = valueWithPullback(at: Float(3), Float(4), of: {
    stringDirect($0, $1, name, false)
  })
  expectEqual(12, product.value)
  expectEqual((20, 15), product.pullback(5))
}
runAllTests()
