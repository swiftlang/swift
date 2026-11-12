// REQUIRES: executable_test
// RUN: %target-run-simple-swift

// The generic seed requires reabstraction when the derivative fixes it to
// Float. Differentiating only x adds an outer subset-parameters thunk.

import _Differentiation

@inline(__always)
func makePullback<Seed: BinaryFloatingPoint, T: AdditiveArithmetic>(
  _: T.Type, _: Seed.Type
) -> (Seed) -> (Float, T) {
  { seed in (2 * Float(seed), T.zero) }
}

@differentiable(reverse, wrt: (x, y))
@inline(never)
public func mixed<T: Differentiable & AdditiveArithmetic>(_ x: Float, _ y: T) -> Float
where T.TangentVector == T { 2 * x }

@derivative(of: mixed, wrt: (x, y))
@inline(__always)
public func vjpMixed<T: Differentiable & AdditiveArithmetic>(_ x: Float, _ y: T)
  -> (value: Float, pullback: (Float) -> (Float, T)) where T.TangentVector == T {
  (2 * x, makePullback(T.self, Float.self))
}

@differentiable(reverse, wrt: x)
public func genericSubset<T: Differentiable & AdditiveArithmetic>(_ x: Float, _ y: T, _ flag: Bool) -> Float
where T.TangentVector == T {
  if flag { return mixed(x, y) }
  return x * x
}

// Addition's pullback has no captures. Differentiating only x still requires
// the subset-parameters thunk.
@differentiable(reverse, wrt: x)
public func thinSubset(_ x: Float, _ y: Float, _ flag: Bool) -> Float {
  if flag { return x + y }
  return x * x
}

// CHECK-LABEL: // reverse-mode derivative of genericSubset<A>
// CHECK-NOT: } // end sil function
// CHECK: // function_ref specialized pullback of genericSubset<A>
// CHECK-NEXT: %[[PB:[0-9]+]] = function_ref
// CHECK-NEXT: %[[PULLBACK:[0-9]+]] = partial_apply {{.*}} %[[PB]]{{[<(]}}
// CHECK-NEXT: %[[RESULT:[0-9]+]] = tuple (%{{[0-9]+}}, %[[PULLBACK]])
// CHECK-NOT: } // end sil function
// CHECK: return %[[RESULT]]
// CHECK-NEXT: } // end sil function

// CHECK-LABEL: // specialized pullback of genericSubset<A>
// CHECK-NOT: = partial_apply
// CHECK-NOT: apply {{.*}} : $@callee_guaranteed
// CHECK: } // end sil function

#if !DEFINITIONS_ONLY
import StdlibUnittest
var Tests = TestSuite("adcs_generic_adapter")
Tests.test("SubsetPullback") {
  for flag in [false, true] {
    let floating = valueWithPullback(at: Float(3), of: {
      genericSubset($0, Float(4), flag)
    })
    expectEqual(flag ? 6 : 9, floating.value)
    expectEqual(flag ? 2 : 6, floating.pullback(1))
    expectEqual(flag ? 10 : 30, floating.pullback(5))

    let double = valueWithPullback(at: Float(3), of: {
      genericSubset($0, Double(4), flag)
    })
    expectEqual(flag ? 6 : 9, double.value)
    expectEqual(flag ? 2 : 6, double.pullback(1))
    expectEqual(flag ? 4 : 12, double.pullback(2))

    let thin = valueWithPullback(at: Float(3), of: { thinSubset($0, 4, flag) })
    expectEqual(flag ? 7 : 9, thin.value)
    expectEqual(flag ? 2 : 12, thin.pullback(2))
  }
}
runAllTests()
#endif
