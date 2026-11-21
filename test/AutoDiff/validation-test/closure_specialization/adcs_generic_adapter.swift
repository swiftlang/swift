// REQUIRES: executable_test
// RUN: %target-run-simple-swift
// RUN: %target-swift-frontend -emit-sil -O -sil-verify-all -module-name adcs_generic_adapter %s -o %t.sil
// RUN: %FileCheck %s < %t.sil

// Fixing Seed to Float requires reabstraction. Differentiating only x adds a
// subset-parameters thunk.

import _Differentiation
import StdlibUnittest

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

// Differentiating only x wraps addition's captureless pullback in a subset thunk.
@differentiable(reverse, wrt: x)
public func thinSubset(_ x: Float, _ y: Float, _ flag: Bool) -> Float {
  if flag { return x + y }
  return x * x
}

// CHECK-LABEL: {{^}}// reverse-mode derivative of genericSubset<A>(_:_:_:)
// CHECK:         %[[#GEN_PAYLOAD:]] = tuple $(predecessor: {{[^,]+}}<τ_0_0>, ())
// CHECK-NEXT:    %[[#]] = enum $[[GEN_TAPE:_AD__[^ ,]+_spec[^ ,<]+]]<τ_0_0>, #[[GEN_TAPE]].bb1!enumelt, %[[#GEN_PAYLOAD]]
// CHECK:         // function_ref specialized pullback of genericSubset<A>(_:_:_:)
// CHECK-NEXT:    %[[#GEN_PB:]] = function_ref @[[GEN_PULLBACK:[^ ]+]]
// CHECK-NEXT:    %[[#]] = partial_apply [callee_guaranteed] %[[#GEN_PB]]<τ_0_0>(%[[#]])
// CHECK:       } // end sil function

// CHECK-LABEL: {{^}}// specialized pullback of genericSubset<A>(_:_:_:)
// CHECK:       sil private @[[GEN_PULLBACK]]
// CHECK-NOT:     partial_apply
// CHECK-NOT:     apply {{.*}} : $@callee_guaranteed
// CHECK:       } // end sil function

// CHECK-LABEL: {{^}}// reverse-mode derivative of thinSubset(_:_:_:)
// CHECK:         %[[#THIN_PAYLOAD:]] = tuple $(predecessor: {{[^,]+}}, ())
// CHECK-NEXT:    %[[#]] = enum $[[THIN_TAPE:_AD__[^ ,]+_spec[^ ,]+]], #[[THIN_TAPE]].bb1!enumelt, %[[#THIN_PAYLOAD]]
// CHECK:         // function_ref specialized pullback of thinSubset(_:_:_:)
// CHECK-NEXT:    %[[#THIN_PB:]] = function_ref @[[THIN_PULLBACK:[^ ]+]]
// CHECK-NEXT:    %[[#]] = partial_apply [callee_guaranteed] %[[#THIN_PB]](%[[#]])
// CHECK:       } // end sil function

// CHECK-LABEL: {{^}}// specialized pullback of thinSubset(_:_:_:)
// CHECK:       sil private @[[THIN_PULLBACK]]
// CHECK-NOT:     partial_apply
// CHECK-NOT:     apply {{.*}} : $@callee_guaranteed
// CHECK:       } // end sil function

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
