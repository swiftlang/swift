
// RUN: %target-swift-frontend -module-name specialize_self_conforming -emit-sil -O -primary-file %s | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

@objc protocol P: Q {
  func foo()
}

@objc protocol Q {
  func bar()
}

@_optimize(none)
func takesP<T : P>(_ t: T) {}

@_optimize(none)
func takesQ<T : Q>(_ t: T) {}

@inline(__always)
func callsTakesP<T : P>(_ t: T) {
  takesP(t)
  takesQ(t)
  t.foo()
  t.bar()
}

// CHECK-LABEL: sil hidden @$s26specialize_self_conforming16callsTakesPWithPyyAA1P_pF : $@convention(thin) (@guaranteed any P) -> () {
// CHECK: [[FN:%.*]] = function_ref @$s26specialize_self_conforming6takesPyyxAA1PRzlF : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@guaranteed τ_0_0) -> ()
// CHECK: apply [[FN]]<any P>(%0) : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@guaranteed τ_0_0) -> ()
// CHECK: return

func callsTakesPWithP(_ p: P) {
  callsTakesP(p)
}
