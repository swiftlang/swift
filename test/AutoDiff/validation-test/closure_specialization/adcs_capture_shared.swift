// REQUIRES: executable_test
// RUN: %target-run-simple-swift

// The indirect capture and subset thunk are shared between mutually exclusive
// predecessor payloads. Capture cleanup must not deallocate the same buffer twice.
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

// Differentiate with respect to both inputs so the saved closure has no
// subset thunk. The direct pullback call still needs its indirect String.
@differentiable(reverse, wrt: (x, y))
public func stringDirect(_ x: Float, _ y: Float, _ name: String, _ flag: Bool) -> Float {
  if flag { return labeled(x, y, name) }
  return x * y
}

// CHECK-LABEL: // reverse-mode derivative of stringShared
// CHECK-NOT: } // end sil function
// CHECK: // function_ref specialized pullback of stringShared
// CHECK-NEXT: %[[PB:[0-9]+]] = function_ref
// CHECK-NEXT: %[[PULLBACK:[0-9]+]] = partial_apply {{.*}} %[[PB]]{{[<(]}}
// CHECK-NEXT: %[[RESULT:[0-9]+]] = tuple (%{{[0-9]+}}, %[[PULLBACK]])
// CHECK-NOT: } // end sil function
// CHECK: return %[[RESULT]]
// CHECK-NEXT: } // end sil function

// CHECK-LABEL: // specialized pullback of stringShared
// CHECK-NOT: apply {{.*}} : $@callee_guaranteed
// CHECK: } // end sil function

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
