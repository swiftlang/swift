// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -module-name NestedTape %s -Xllvm -sil-print-around=autodiff-closure-specialization -o %t.sil 2> %t.log
// RUN: %FileCheck %s --check-prefix=BOUND < %t.log

// BOUND-LABEL: {{^}}sil shared {{.*}}@$s10NestedTape06nestedB0yS2f_xmSbtlFlTJrSUUpSrSS_Tg5
// BOUND:       } // end sil function
// BOUND-LABEL: {{^}}sil shared {{.*}}@$s10NestedTape06nestedB0yS2f_xmSbtlFlTJrSUUpSrSS_Tg5

// BOUND:      bb1(
// BOUND:        %[[#OUTER_REF:]] = function_ref @[[OUTER_PB:[^ ]+]] : $@convention(thin) (Float, @owned {{_AD__[^ )]+_spec[^ )]+}}) -> Float
// BOUND-NEXT:   %[[#RETURNED_PB:]] = partial_apply [callee_guaranteed] %[[#OUTER_REF]]
// BOUND-NEXT:   %[[#]] = tuple (%[[#]], %[[#RETURNED_PB]])
// BOUND-NEXT:   return

// BOUND:      bb2:
// BOUND-NOT:    = partial_apply
// BOUND:        %[[#ADD:]] = thin_to_thick_function {{.*}} to $@callee_guaranteed (Float) -> (Float, Float)
// BOUND-NEXT:   %[[#INNER_PAYLOAD:]] = tuple $(predecessor: {{_AD__[^, ]+}}<Int>, @callee_guaranteed (Float) -> (Float, Float)) ({{.*}}%[[#ADD]])
// BOUND-NEXT:   %[[#INNER_TAPE:]] = enum $[[INNER_TYPE:_AD__[^, ]+<Int>]], {{.*}}%[[#INNER_PAYLOAD]]
// BOUND-NOT:    = partial_apply
// BOUND:        function_ref @[[INNER_PB:[^ ]+]] : $@convention(thin) (Float, @owned [[INNER_TYPE]]) -> Float
// BOUND-NOT:    = partial_apply
// BOUND:      tuple (%[[#INNER_TAPE]])
// BOUND-NOT:    = partial_apply
// BOUND:      bb3:
// BOUND-NOT:    = partial_apply
// BOUND:      } // end sil function

// BOUND:      sil private [ossa] @[[OUTER_PB]]
// BOUND-NOT:    = partial_apply
// BOUND:        %[[#DIRECT:]] = function_ref @[[INNER_PB]]
// BOUND-NEXT:   %[[#]] = apply %[[#DIRECT]]

import _Differentiation

// Early inlining nests the Int branch tape inside the String and Double
// VJPs. ADCS must preserve its captureless callback and enum type.
@differentiable(reverse, wrt: x)
@inline(never)
public func nestedTape<T>(_ x: Float, _ type: T.Type, _ flag: Bool) -> Float {
  if T.self == Int.self { return x + x }
  if flag { return nestedTape(x * x, Int.self, false) }
  return x * x
}

@differentiable(reverse, wrt: x)
@inline(never)
public func stringNestedTape(_ x: Float, _ flag: Bool) -> Float {
  nestedTape(x, String.self, flag)
}

@differentiable(reverse, wrt: x)
@inline(never)
public func doubleNestedTape(_ x: Float, _ flag: Bool) -> Float {
  nestedTape(x, Double.self, flag)
}
