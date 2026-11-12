// REQUIRES: executable_test
// RUN: %target-run-simple-swift

// The generic root closure captures a metatype and retains substitutions
// when ADCS emits its direct pullback call.

import _Differentiation
import StdlibUnittest

@differentiable(reverse, wrt: x)
public func tagged<T>(_ x: Float, _ tag: T.Type) -> Float {
  tag == Int.self ? 2 * x : 3 * x
}

@derivative(of: tagged, wrt: x)
@inline(__always)
public func vjpTagged<T>(
  _ x: Float, _ tag: T.Type
) -> (value: Float, pullback: (Float) -> Float) {
  (tagged(x, tag), { seed in tag == Int.self ? 2 * seed : 3 * seed })
}

@differentiable(reverse, wrt: x)
public func genericBranch<T>(_ x: Float, _ tag: T.Type, _ flag: Bool) -> Float {
  if flag { return tagged(x, tag) }
  return tagged(x, tag) + x
}

// The generic derivative remains available to unspecialized callers. Replacing
// its closure call must retain the generic substitutions on vjpTagged's closure.
// CHECK-LABEL: // reverse-mode derivative of genericBranch<A>
// CHECK-NOT: } // end sil function
// CHECK: // function_ref specialized pullback of genericBranch<A>
// CHECK-NEXT: %[[PB:[0-9]+]] = function_ref
// CHECK-NEXT: %[[PULLBACK:[0-9]+]] = partial_apply {{.*}} %[[PB]]{{[<(]}}
// CHECK-NEXT: %[[RESULT:[0-9]+]] = tuple (%{{[0-9]+}}, %[[PULLBACK]])
// CHECK-NOT: } // end sil function
// CHECK: return %[[RESULT]]
// CHECK-NEXT: } // end sil function

// CHECK-LABEL: // specialized pullback of genericBranch<A>
// CHECK: sil {{.*}} <{{.*}}> (Float,
// CHECK-NOT: apply {{.*}} : $@callee_guaranteed
// CHECK: } // end sil function

var tests = TestSuite("ADCSGenericCapture")
tests.test("MetatypeCapture") {
  let integer = valueWithPullback(at: Float(3), of: {
    genericBranch($0, Int.self, true)
  })
  expectEqual(6, integer.value)
  expectEqual(10, integer.pullback(5))
  expectEqual(4, integer.pullback(2))

  let floating = valueWithPullback(at: Float(3), of: {
    genericBranch($0, Double.self, true)
  })
  expectEqual(9, floating.value)
  expectEqual(15, floating.pullback(5))

  let additive = valueWithPullback(at: Float(3), of: {
    genericBranch($0, Int.self, false)
  })
  expectEqual(9, additive.value)
  expectEqual(15, additive.pullback(5))
}
runAllTests()
