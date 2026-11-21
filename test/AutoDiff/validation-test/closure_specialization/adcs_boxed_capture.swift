// REQUIRES: executable_test
// RUN: %target-run-simple-swift(-Onone)
// RUN: %target-run-simple-swift(-O)
// RUN: %target-swift-frontend -emit-sil -O -sil-verify-all -module-name adcs_boxed_capture %s -o %t.sil
// RUN: %FileCheck %s < %t.sil

import _Differentiation
import StdlibUnittest

@differentiable(reverse)
@inline(never)
public func boxedSquare(_ x: Float) -> Float { x * x }

@derivative(of: boxedSquare)
@inline(__always)
public func boxedSquareVJP(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
  var factor = 2 * x
  return (x * x, { seed in
    // The assignment keeps the capture boxed.
    factor = 2 * x
    return factor * seed
  })
}

@differentiable(reverse, wrt: x)
public func branch(_ x: Float, _ flag: Bool) -> Float {
  if flag { return boxedSquare(x) }
  return x * x * x
}

// CHECK-LABEL: {{^}}// reverse-mode derivative of branch(_:_:)
// CHECK:         %[[#BOX:]] = alloc_box ${ var Float }
// CHECK:         // function_ref closure #1 in boxedSquareVJP(_:)
// CHECK-NEXT:    %[[#ROOT:]] = function_ref
// CHECK-NEXT:    %[[#CALLBACK:]] = partial_apply [callee_guaranteed] %[[#ROOT]](%[[#BOX]],
// CHECK-NEXT:    %[[#BOXED_PAYLOAD:]] = tuple {{.*}}%[[#CALLBACK]])
// CHECK-NEXT:    %[[#]] = enum $[[TAPE:_AD__[^ ,]+_spec[^ ,]+]], #[[TAPE]].bb1!enumelt, %[[#BOXED_PAYLOAD]]
// CHECK-NOT:     partial_apply
// CHECK:         %[[#CAPTURE_PAYLOAD:]] = tuple $(predecessor: {{[^,]+}}, (Float, Float), (Float, Float))
// CHECK-NEXT:    %[[#]] = enum $[[TAPE]], #[[TAPE]].bb2!enumelt, %[[#CAPTURE_PAYLOAD]]
// CHECK:         // function_ref specialized pullback of branch(_:_:)
// CHECK-NEXT:    %[[#PB:]] = function_ref @[[PULLBACK:[^ ]+]]
// CHECK-NEXT:    %[[#]] = partial_apply [callee_guaranteed] %[[#PB]]
// CHECK:       } // end sil function

// CHECK-LABEL: {{^}}// specialized pullback of branch(_:_:)
// CHECK:       sil private @[[PULLBACK]]
// CHECK:       bb[[#]](%[[#PAYLOAD:]] : $(predecessor: {{[^,]+}}, @callee_guaranteed (Float) -> Float))
// CHECK-NEXT:    %[[#SAVED:]] = tuple_extract %[[#PAYLOAD]], 1
// CHECK-NEXT:    %[[#]] = apply %[[#SAVED]]({{.*}}) : $@callee_guaranteed (Float) -> Float
// CHECK:       } // end sil function

var tests = TestSuite("ADCSBoxedCapture")
tests.test("RepeatedPullbackApplications") {
  for x: Float in [-3, 2] {
    for flag in [false, true] {
      let result = valueWithPullback(at: x, of: { branch($0, flag) })
      expectEqual(flag ? x * x : x * x * x, result.value)
      let derivative = flag ? 2 * x : 3 * x * x
      for seed: Float in [1, 2, -1] {
        expectEqual(derivative * seed, result.pullback(seed))
      }
    }
  }
}
runAllTests()
