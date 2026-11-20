// REQUIRES: executable_test
// RUN: %target-run-simple-swift
// RUN: %target-swift-frontend -emit-sil -O -sil-verify-all -module-name src %s -o %t.sil
// RUN: %FileCheck %s < %t.sil

// The outer closure has a static entry point and captures the runtime wrapper
// and the inner pullback.

import _Differentiation
import StdlibUnittest

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

// CHECK-LABEL: // reverse-mode derivative of branchDynamic(_:_:_:_:)
// CHECK:         // function_ref closure #1 in vjpRuntimeWrapped(_:_:_:)
// CHECK-NEXT:    %[[#INNER_FN:]] = function_ref
// CHECK-NEXT:    %[[#INNER:]] = partial_apply [callee_guaranteed] %[[#INNER_FN]]
// CHECK-NOT:     = partial_apply
// CHECK:         %[[#CAPTURES:]] = tuple (%[[#]], %[[#INNER]])
// CHECK-NEXT:    %[[#PAYLOAD:]] = tuple {{.*}} (%[[#]], %[[#CAPTURES]])
// CHECK-NEXT:    enum $[[BTE:_AD__[^, ]+_spec[^, ]+]], {{.*}} %[[#PAYLOAD]]
// CHECK-NOT:     = partial_apply
// CHECK:         // function_ref specialized pullback of branchDynamic(_:_:_:_:)
// CHECK-NEXT:    %[[#OUTER_FN:]] = function_ref
// CHECK-NEXT:    %[[#OUTER:]] = partial_apply [callee_guaranteed] %[[#OUTER_FN]](%[[#]]) : $@convention(thin) (Float, @owned [[BTE]]) -> Float
// CHECK-NEXT:    %[[#RESULT:]] = tuple (%[[#]], %[[#OUTER]])
// CHECK-NEXT:    return %[[#RESULT]]
// CHECK-NEXT:  } // end sil function

// CHECK-LABEL: // specialized pullback of branchDynamic(_:_:_:_:)
// CHECK-NOT:     = partial_apply
// CHECK:         switch_enum %[[#]], case #[[BTE]].bb2!enumelt: bb[[#]], case #[[BTE]].bb1!enumelt: bb[[#WRAPPED_CASE:]]
// CHECK-NOT:     = partial_apply
// CHECK:       bb[[#WRAPPED_CASE]](%[[#WRAPPED_PAYLOAD:]] :
// CHECK-NEXT:    %[[#STORED:]] = tuple_extract %[[#WRAPPED_PAYLOAD]], 1
// CHECK-NEXT:    %[[#WRAPPER:]] = tuple_extract %[[#STORED]], 0
// CHECK-NEXT:    %[[#SAVED_INNER:]] = tuple_extract %[[#STORED]], 1
// CHECK:         %[[#NOESCAPE:]] = convert_escape_to_noescape %[[#SAVED_INNER]]
// CHECK-NEXT:    apply %[[#WRAPPER]](%[[#]], %[[#NOESCAPE]])
// CHECK-NOT:     = partial_apply
// CHECK:       } // end sil function

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
