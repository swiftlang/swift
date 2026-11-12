// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -module-name Identity %S/../../validation-test/closure_specialization/adcs_generic_bte_context.swift -Xllvm -sil-print-around=autodiff-closure-specialization -o %t.sil 2> %t.passes
// RUN: %FileCheck %S/../../validation-test/closure_specialization/adcs_generic_bte_context.swift < %t.sil
// RUN: %FileCheck %s --check-prefix=BOUND < %t.passes

// The source creates concrete VJPs with an empty generic signature, while the
// original tape declaration still has its phantom generic parameter. After
// ADCS the Int and String instantiations must have distinct nominal tapes.
// BOUND-LABEL: sil shared {{.*}}@$s8Identity6branchyS2f_xmtlFlTJrSUpSrSi_Tt0g5
// BOUND: enum ${{_AD__[^, ]+}}<Int>,
// BOUND: function_ref @[[ORIGINAL_PB:[^ ]+]] : $@convention(thin) (Float, @owned {{_AD__[^ )]+}}<Int>) -> Float
// BOUND-LABEL: sil shared {{.*}}@$s8Identity6branchyS2f_xmtlFlTJrSUpSrSi_Tt0g5
// BOUND: enum $[[INT_TAPE:_AD__[^, ]+_spec[^, ]+]],
// BOUND: function_ref @[[DROPPED_PB:[^ ]+]] : $@convention(thin) (Float, @owned [[INT_TOP_TAPE:_AD__[^ )]+_spec[^ )]+]]) -> Float
// BOUND: } // end sil function
// BOUND-LABEL: sil shared {{.*}}@$s8Identity6branchyS2f_xmtlFlTJrSUpSrSS_Tt0g5
// BOUND: enum ${{_AD__[^, ]+}}<String>,
// BOUND-LABEL: sil shared {{.*}}@$s8Identity6branchyS2f_xmtlFlTJrSUpSrSS_Tt0g5
// BOUND-NOT: [[INT_TAPE]]
// BOUND: enum ${{_AD__[^, ]+_spec[^, ]+}},
// BOUND-NOT: [[INT_TAPE]]
// BOUND: } // end sil function

// Both Int VJPs use the same original pullback and specialize to a shared tape
// declaration. Their specialized pullbacks have separate VJP-scoped identities.
// BOUND-LABEL: sil shared {{.*}}@$s8Identity6branchyS2f_xmtlFlTJrSUpSrSi_Tg5
// BOUND: enum ${{_AD__[^, ]+}}<Int>,
// BOUND: function_ref @[[ORIGINAL_PB]] : $@convention(thin)
// BOUND-LABEL: sil shared {{.*}}@$s8Identity6branchyS2f_xmtlFlTJrSUpSrSi_Tg5
// BOUND: enum $[[INT_TAPE]],
// BOUND-NOT: function_ref @[[DROPPED_PB]]
// BOUND: : $@convention(thin) (Float, @owned [[INT_TOP_TAPE]]) -> Float
// BOUND: } // end sil function
