// REQUIRES: executable_test
// RUN: %target-run-simple-swift
// RUN: %target-swift-frontend -emit-sil -O -sil-verify-all -module-name adcs_capture_generic %s -o %t.sil
// RUN: %FileCheck %s < %t.sil

// The captured metatype exercises the direct call's generic substitutions.

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

// CHECK-LABEL: {{^}}// reverse-mode derivative of genericBranch<A>(_:_:_:)
// CHECK:       bb0(%[[#]] : $Float, %[[#TAG:]] : $@thick τ_0_0.Type,
// CHECK-NOT:     partial_apply
// CHECK:         %[[#CAPTURES:]] = tuple (%[[#TAG]])
// CHECK-NEXT:    %[[#PAYLOAD:]] = tuple $(predecessor: {{[^,]+}}, (_: @thick τ_0_0.Type)) (%[[#]], %[[#CAPTURES]])
// CHECK-NEXT:    %[[#]] = enum $[[TAPE:_AD__[^ ,]+_spec[^ ,<]+]]<τ_0_0>, #[[TAPE]].bb1!enumelt, %[[#PAYLOAD]]
// CHECK-NOT:     partial_apply
// CHECK:         // function_ref specialized pullback of genericBranch<A>(_:_:_:)
// CHECK-NEXT:    %[[#PB:]] = function_ref @[[PULLBACK:[^ ]+]] : $@convention(thin) <τ_0_0> (Float, @owned [[TAPE]]<τ_0_0>) -> Float
// CHECK-NEXT:    %[[#]] = partial_apply [callee_guaranteed] %[[#PB]]<τ_0_0>(%[[#]])
// CHECK:       } // end sil function

// CHECK-LABEL: {{^}}// specialized pullback of genericBranch<A>(_:_:_:)
// CHECK:       sil private @[[PULLBACK]]
// CHECK-NOT:     apply {{.*}} : $@callee_guaranteed
// CHECK:       } // end sil function

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
