// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -module-name TapeScope -Xllvm -sil-print-around=autodiff-closure-specialization %s -o /dev/null 2> %t
// RUN: %FileCheck %s < %t

import _Differentiation

@differentiable(reverse, wrt: x)
@inline(__always)
public func nestedTape<T>(_ x: Float, _ type: T.Type, _ flag: Bool) -> Float {
  if T.self == Int.self { return x * x }
  if flag { return nestedTape(x * x, Int.self, false) }
  return x * x
}

@differentiable(reverse, wrt: x)
@inline(never)
public func stringNestedTape(_ x: Float, _ flag: Bool) -> Float {
  nestedTape(x, String.self, flag)
}

// Both tapes come from the same primal. Specializing the String VJP's
// outer tape must leave the inlined Int tape intact.
// CHECK-LABEL: {{^}}sil shared {{.*}}@$s9TapeScope06nestedA0yS2f_xmSbtlFlTJrSUUpSrSS_Tg5 :
// CHECK:       } // end sil function
// CHECK-LABEL: {{^}}sil shared {{.*}}@$s9TapeScope06nestedA0yS2f_xmSbtlFlTJrSUUpSrSS_Tg5 :
// CHECK:         %[[#OUTER_FN:]] = function_ref @[[OUTER_PB:[^ ]+]] : $@convention(thin) (Float, @owned [[OUTER_TAPE:_AD__[^ ]+_spec[^ )]+]]) -> Float
// CHECK-NEXT:    %[[#]] = partial_apply [callee_guaranteed] %[[#OUTER_FN]]
// CHECK:         %[[#INNER_FN:]] = function_ref @$sSf16_DifferentiationE12_vjpMultiply
// CHECK:         %[[#INNER_CALLBACK:]] = partial_apply [callee_guaranteed] %[[#INNER_FN]]({{.*}}) : $@convention(thin) (Float, Float, Float) -> (Float, Float)
// CHECK-NEXT:    %[[#INNER_PAYLOAD:]] = tuple $(predecessor: {{_AD__[^,]+}}<Int>, @callee_guaranteed (Float) -> (Float, Float)) (%[[#]], %[[#INNER_CALLBACK]])
// CHECK-NEXT:    %[[#INNER_ENUM:]] = enum $[[INNER_TAPE:_AD__[^,]+<Int>]], {{.*}}%[[#INNER_PAYLOAD]]
// CHECK:         function_ref @[[INNER_PB:[^ ]+]] : $@convention(thin) (Float, @owned [[INNER_TAPE]]) -> Float
// CHECK:         %[[#CAPTURES:]] = tuple (%[[#]], %[[#]])
// CHECK-NEXT:    %[[#NESTED_CAPTURE:]] = tuple (%[[#INNER_ENUM]])
// CHECK-NEXT:    %[[#OUTER_PAYLOAD:]] = tuple {{.*}}(Float, Float), (_: [[INNER_TAPE]])) (%[[#]], %[[#CAPTURES]], %[[#NESTED_CAPTURE]])
// CHECK-NEXT:  enum $[[OUTER_TAPE]], {{.*}}%[[#OUTER_PAYLOAD]]
// CHECK:       } // end sil function

// CHECK:      sil private [ossa] @[[OUTER_PB]]
// CHECK:        %[[#DIRECT:]] = function_ref @[[INNER_PB]]
// CHECK-NEXT:   %[[#]] = apply %[[#DIRECT]]({{.*}}) : $@convention(thin) (Float, @owned [[INNER_TAPE]]) -> Float
