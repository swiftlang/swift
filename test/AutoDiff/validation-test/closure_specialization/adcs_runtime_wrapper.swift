// REQUIRES: executable_test
// RUN: %target-run-simple-swift

// Swift closure expressions lower to a static closure entry point capturing
// the runtime wrapper and inner pullback. ADCS removes that outer closure.

import _Differentiation

public typealias Pullback = (Float) -> Float
public typealias Wrapper = (Float, Pullback) -> Float

@differentiable(reverse, wrt: x)
@inline(never)
public func runtimeWrapped(_ x: Float, _ label: String, _ wrapper: @escaping Wrapper) -> Float { 2*x }

@derivative(of: runtimeWrapped, wrt: x)
@inline(__always)
public func vjpRuntimeWrapped(_ x: Float, _ label: String, _ wrapper: @escaping Wrapper)
  -> (value: Float, pullback: Pullback) {
  let pullback: Pullback = { seed in withExtendedLifetime(label) { 2*seed } }
  return (2*x, { seed in wrapper(seed, pullback) })
}

@differentiable(reverse, wrt: x)
public func branchDynamic(_ x: Float, _ label: String, _ flag: Bool, _ wrapper: @escaping Wrapper) -> Float {
  if flag { return runtimeWrapped(x, label, wrapper) }
  return x*x
}


// CHECK-LABEL: // reverse-mode derivative of branchDynamic
// CHECK-NOT: } // end sil function
// CHECK: // function_ref specialized pullback of branchDynamic
// CHECK-NEXT: %[[PB:[0-9]+]] = function_ref
// CHECK-NEXT: %[[PULLBACK:[0-9]+]] = partial_apply {{.*}} %[[PB]]{{[<(]}}
// CHECK-NEXT: %[[RESULT:[0-9]+]] = tuple (%{{[0-9]+}}, %[[PULLBACK]])
// CHECK-NOT: } // end sil function
// CHECK: return %[[RESULT]]
// CHECK-NEXT: } // end sil function

// CHECK-LABEL: // specialized pullback of branchDynamic
// CHECK-NOT: = partial_apply
// CHECK-NOT: apply {{.*}} : $@callee_guaranteed (Float) -> Float
// CHECK: } // end sil function

#if !DEFINITIONS_ONLY
import StdlibUnittest
var Tests = TestSuite("adcs_runtime_wrapper")
Tests.test("Pullback") {
  for flag in [false, true] {

    var calls = 0
    let wrapper: Wrapper = { seed, pullback in
      calls += 1
      return pullback(seed)
    }
    let (value, pullback) = valueWithPullback(at: Float(3), of: {
      branchDynamic($0, "a label longer than a small string", flag, wrapper)
    })
    expectEqual(flag ? 6 : 9, value)
    expectEqual(flag ? 2 : 6, pullback(1))
    expectEqual(flag ? 4 : 12, pullback(2))
    expectEqual(flag ? 2 : 0, calls)

  }
}
runAllTests()
#endif
