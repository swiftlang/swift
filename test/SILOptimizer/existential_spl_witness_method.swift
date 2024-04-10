// RUN: %target-swift-frontend -O -Xllvm -sil-disable-pass=GenericSpecializer -Xllvm -sil-disable-pass=EarlyInliner -Xllvm -sil-disable-pass=PerfInliner -Xllvm -sil-disable-pass=LateInliner -emit-sil -sil-verify-all %s | %FileCheck %s

// Test for ExistentialSpecializer when an existential type is passed to a witness_method func representation
protocol P {
@inline(never)
  func myfuncP(_ q:Q) -> Int
}

protocol Q {
@inline(never)
  func myfuncQ() -> Int
}

class C : P {
  var id = 10
@inline(never)
  func myfuncP(_ q:Q) -> Int {
    return id
  }
}

class D : Q {
  var id = 20
@inline(never)
  func myfuncQ() -> Int {
    return id
  }
}

// CHECK-LABEL: @$s30existential_spl_witness_method1CCAA1PA2aDP7myfuncPySiAA1Q_pFTW : $@convention(witness_method: P) (@in_guaranteed any Q, @in_guaranteed C) -> Int {
// CHECK: [[FR1:%.*]] = function_ref @$s30existential_spl_witness_method1CCAA1PA2aDP7myfuncPySiAA1Q_pFTWTf4en_n : $@convention(thin) <τ_0_0 where τ_0_0 : Q> (@in_guaranteed τ_0_0, @in_guaranteed C) -> Int
// CHECK: apply [[FR1]]
// CHECK-LABEL: } // end sil function '$s30existential_spl_witness_method1CCAA1PA2aDP7myfuncPySiAA1Q_pFTW'

// CHECK-LABEL: @$s30existential_spl_witness_method3bazyyAA1P_p_AA1Q_ptFTf4ee_n : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : P, τ_0_1 : Q> (@in_guaranteed τ_0_0, @in_guaranteed τ_0_1) -> () {
// CHECK: [[FR2:%.*]] = function_ref @$s30existential_spl_witness_method1CCAA1PA2aDP7myfuncPySiAA1Q_pFTW : $@convention(witness_method: P) (@in_guaranteed any Q, @in_guaranteed C) -> Int
// CHECK: apply [[FR2]]
// CHECK-LABEL: } // end sil function '$s30existential_spl_witness_method3bazyyAA1P_p_AA1Q_ptFTf4ee_n'
@inline(never)
func baz(_ p : P, _ q : Q) {
  p.myfuncP(q)
}

baz(C(), D());

