// RUN: %target-swift-frontend -module-name foo -emit-silgen %s | %FileCheck %s
// RUN: %target-swift-frontend -module-name foo -emit-sil -verify %s

// CHECK-LABEL: sil @main

// CHECK-LABEL: sil shared @_TFF3foog5recurFT_T_U_FT_T_
var recur : () -> () {
  // CHECK-LABEL: function_ref @_TF3foog5recurFT_T_
  return { recur() } // expected-warning {{attempting to access 'recur' within its own getter}}
}

// CHECK-LABEL: sil shared @_TFF3foog12recur_harderFFT_T_FT_T_U_FFT_T_FT_T_
var recur_harder : (() -> ()) -> (() -> ()) {
  // CHECK-LABEL: function_ref @_TF3foog12recur_harderFFT_T_FT_T_
  return { f in recur_harder(f) } // expected-warning {{attempting to access 'recur_harder' within its own getter}}
}
