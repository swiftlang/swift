// RUN: %target-swift-frontend -emit-sil -O -primary-file %s | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

@objc protocol P {}

@_optimize(none)
func takesP<T : P>(_: T) {}

@inline(__always)
func callsTakesP<T : P>(_ t: T) {
  takesP(t)
}

// CHECK-LABEL: sil hidden @_T026specialize_self_conforming16callsTakesPWithPyAA1P_pF : $@convention(thin) (@owned P) -> () {
// CHECK: [[FN:%.*]] = function_ref @_T026specialize_self_conforming6takesPyxAA1PRzlF : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@owned τ_0_0) -> ()
// CHECK: apply [[FN]]<P>(%0) : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@owned τ_0_0) -> ()
// CHECK: return

func callsTakesPWithP(_ p: P) {
  callsTakesP(p)
}
