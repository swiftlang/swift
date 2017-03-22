// RUN: %target-swift-frontend -module-name foo -emit-silgen %s | %FileCheck %s
// RUN: %target-swift-frontend -module-name foo -emit-sil -verify %s

// CHECK-LABEL: sil @main

// CHECK-LABEL: sil shared @_T03foo5recuryycfgyycfU_
var recur : () -> () {
  // CHECK-LABEL: function_ref @_T03foo5recuryycfg
  return { recur() } // expected-warning {{attempting to access 'recur' within its own getter}}
}

// CHECK-LABEL: sil shared @_T03foo12recur_harderyycyyccfgyycyyccfU_
var recur_harder : (() -> ()) -> (() -> ()) {
  // CHECK-LABEL: function_ref @_T03foo12recur_harderyycyyccfg
  return { f in recur_harder(f) } // expected-warning {{attempting to access 'recur_harder' within its own getter}}
}
