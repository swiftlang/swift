// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/witness_accessibility_other.swiftmodule %S/Inputs/witness_accessibility_other.swift
// RUN: %target-swift-emit-silgen -I %t  -enable-sil-ownership %s | %FileCheck %s
// RUN: %target-swift-emit-sil -I %t -enable-sil-ownership %s

import witness_accessibility_other

// We can see the conformance S : P, but we cannot see the witness for
// P.publicRequirement() because it is defined in an extension of a
// private protocol R to which S also conforms.
//
// So make sure the witness is invoked via virtual dispatch even with
// a concrete base type.

// CHECK-LABEL: sil @$S27witness_accessibility_multi22callsPublicRequirement1sy0a1_B6_other1SV_tF : $@convention(thin) (S) -> ()
public func callsPublicRequirement(s: S) {

  // CHECK: witness_method $S, #P.publicRequirement!1 : <Self where Self : P> (Self) -> () -> () : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> ()
  s.publicRequirement()

  // CHECK: function_ref @$S27witness_accessibility_other1QPAAE19internalRequirementyyF : $@convention(method) <τ_0_0 where τ_0_0 : Q> (@in_guaranteed τ_0_0) -> ()
  s.internalRequirement()

  // CHECK: return
}
