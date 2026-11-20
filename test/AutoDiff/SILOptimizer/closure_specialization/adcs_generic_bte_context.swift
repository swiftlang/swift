// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -module-name Identity %s -Xllvm -sil-print-around=autodiff-closure-specialization -o %t.sil 2> %t.passes
// RUN: %FileCheck %s < %t.sil
// RUN: %FileCheck %s --check-prefix=BOUND < %t.passes

// The VJPs are already concrete, but their tape declaration still has a
// phantom generic parameter. Int and String require distinct specialized tapes.
// BOUND-LABEL: {{^}}sil shared {{.*}}@$s8Identity6branchyS2f_xmtlFlTJrSUpSrSi_Tt0g5
// BOUND:       enum ${{_AD__[^, ]+}}<Int>,
// BOUND:         function_ref @[[ORIGINAL_PB:[^ ]+]] : $@convention(thin) (Float, @owned {{_AD__[^ )]+}}<Int>) -> Float
// BOUND-LABEL: {{^}}sil shared {{.*}}@$s8Identity6branchyS2f_xmtlFlTJrSUpSrSi_Tt0g5
// BOUND:       enum $[[INT_TAPE:_AD__[^, ]+_spec[^, ]+]],
// BOUND:         function_ref @[[DROPPED_PB:[^ ]+]] : $@convention(thin) (Float, @owned [[INT_TOP_TAPE:_AD__[^ )]+_spec[^ )]+]]) -> Float
// BOUND:       } // end sil function
// BOUND-LABEL: {{^}}sil shared {{.*}}@$s8Identity6branchyS2f_xmtlFlTJrSUpSrSS_Tt0g5
// BOUND:       enum ${{_AD__[^, ]+}}<String>,
// BOUND-LABEL: {{^}}sil shared {{.*}}@$s8Identity6branchyS2f_xmtlFlTJrSUpSrSS_Tt0g5
// BOUND-NOT:   [[INT_TAPE]]
// BOUND:       enum ${{_AD__[^, ]+_spec[^, ]+}},
// BOUND-NOT:   [[INT_TAPE]]
// BOUND:       } // end sil function

// BOUND-LABEL: {{^}}sil shared {{.*}}@$s8Identity6branchyS2f_xmtlFlTJrSUpSrSi_Tg5
// BOUND:       enum ${{_AD__[^, ]+}}<Int>,
// BOUND:         function_ref @[[ORIGINAL_PB]] : $@convention(thin)
// BOUND-LABEL: {{^}}sil shared {{.*}}@$s8Identity6branchyS2f_xmtlFlTJrSUpSrSi_Tg5
// BOUND:       enum $[[INT_TAPE]],
// BOUND-NOT:     function_ref @[[DROPPED_PB]]
// BOUND:       : $@convention(thin) (Float, @owned [[INT_TOP_TAPE]]) -> Float
// BOUND:       } // end sil function

import _Differentiation

@differentiable(reverse, wrt: x)
@inline(never)
func branch<T>(_ x: Float, _ type: T.Type) -> Float {
  if x > 0 { return x * x }
  return x + x
}

// CHECK: enum _AD__{{.*}}branch{{.*}}_spec{{.*}} {
// CHECK: case bb1((predecessor: {{.*}}, (Float, Float)))

public func intGradient(_ x: Float) -> Float {
  gradient(at: x, of: { branch($0, Int.self) })
}

public func stringGradient(_ x: Float) -> Float {
  gradient(at: x, of: { branch($0, String.self) })
}
