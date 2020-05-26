// RUN: %target-swift-emit-silgen -module-name foo %s | %FileCheck %s
// RUN: %target-swift-emit-sil -module-name foo -verify %s
// RUN: %target-swift-frontend -emit-sil -module-name foo -verify %s -enable-ownership-stripping-after-serialization

// CHECK-LABEL: sil [ossa] @main

// CHECK-LABEL: sil private [ossa] @$s3foo5recuryycvgyycfU_
var recur : () -> () {
  // CHECK-LABEL: function_ref @$s3foo5recuryycvg
  return { recur() } // expected-warning {{attempting to access 'recur' within its own getter}}
}

// CHECK-LABEL: sil private [ossa] @$s3foo12recur_harderyyycyyXEcvgyycyyXEcfU_
var recur_harder : (() -> ()) -> (() -> ()) {
  // CHECK-LABEL: function_ref @$s3foo12recur_harderyyycyyXEcvg
  return { f in recur_harder(f) } // expected-warning {{attempting to access 'recur_harder' within its own getter}}
}
