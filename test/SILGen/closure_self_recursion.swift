// RUN: %target-swift-frontend -module-name foo -enable-sil-ownership -emit-silgen %s | %FileCheck %s
// RUN: %target-swift-frontend -module-name foo -enable-sil-ownership -emit-sil -verify %s

// CHECK-LABEL: sil @main

// CHECK-LABEL: sil private @_T03foo5recuryycvgyycfU_
var recur : () -> () {
  // CHECK-LABEL: function_ref @_T03foo5recuryycvg
  return { recur() } // expected-warning {{attempting to access 'recur' within its own getter}}
}

// CHECK-LABEL: sil private @_T03foo12recur_harderyycyyccvgyycyyccfU_
var recur_harder : (() -> ()) -> (() -> ()) {
  // CHECK-LABEL: function_ref @_T03foo12recur_harderyycyyccvg
  return { f in recur_harder(f) } // expected-warning {{attempting to access 'recur_harder' within its own getter}}
}
