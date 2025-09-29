// RUN: %target-swift-frontend -Xllvm -sil-print-types -Xllvm -sil-print-after=definite-init -emit-sil -module-name assign_or_init_lowering %s -o /dev/null 2>&1 | %FileCheck %s

struct S1<T> {
}

extension S1 where T == Int {
  class Test {
    var test1: Int {
      init(initialValue) { }
      set {}
      get { 0 }
    }

    var test2: T {
      init(initialValue) { }
      set {}
      get { 0 }
    }

    // CHECK-LABEL: sil hidden [ossa] @$s23assign_or_init_lowering2S1VAASiRszlE4TestCAEySi_Gycfc : $@convention(method) (@owned S1<Int>.Test) -> @owned S1<Int>.Test
    //
    // CHECK: [[TEST1_INIT_REF:%.*]] = function_ref @$s23assign_or_init_lowering2S1VAASiRszlE4TestC5test1Sivi : $@convention(thin) (Int, @thick S1<Int>.Test.Type) -> ()
    // CHECK-NEXT: [[SELF:%.*]] = value_metatype $@thick S1<Int>.Test.Type, {{.*}} : $S1<Int>.Test
    // CHECK-NEXT: [[TEST1_INIT_REF_WITH_SELF_APPLIED:%.*]] = partial_apply [callee_guaranteed] [on_stack] [[TEST1_INIT_REF]]([[SELF]]) : $@convention(thin) (Int, @thick S1<Int>.Test.Type) -> ()
    // CHECK: assign_or_init [init] #S1.Test.test1, self {{.*}} : $S1<Int>.Test, value {{.*}} : $Int, init [[TEST1_INIT_REF_WITH_SELF_APPLIED]] : $@noescape @callee_guaranteed (Int) -> (), set {{.*}} : $@noescape @callee_guaranteed (Int) -> ()
    //
    // CHECK: [[TEST2_INIT_REF:%.*]] = function_ref @$s23assign_or_init_lowering2S1VAASiRszlE4TestC5test2Sivi : $@convention(thin) (Int, @thick S1<Int>.Test.Type) -> ()
    // CHECK-NEXT: [[SELF:%.*]] = value_metatype $@thick S1<Int>.Test.Type, {{.*}} : $S1<Int>.Test
    // CHECK-NEXT: [[TEST2_INIT_REF_WITH_SELF_APPLIED:%.*]] = partial_apply [callee_guaranteed] [on_stack] [[TEST2_INIT_REF]]([[SELF]]) : $@convention(thin) (Int, @thick S1<Int>.Test.Type) -> ()
    // CHECK: assign_or_init [init] #S1.Test.test2, self {{.*}} : $S1<Int>.Test, value {{.*}} : $Int, init [[TEST2_INIT_REF_WITH_SELF_APPLIED]] : $@noescape @callee_guaranteed (Int) -> (), set {{.*}} : $@noescape @callee_guaranteed (Int) -> ()
    // CHECK: } // end sil function '$s23assign_or_init_lowering2S1VAASiRszlE4TestCAEySi_Gycfc'
    init() {
      test1 = 0
      test2 = 1
    }
  }
}

struct S2<T, U> {
}

extension S2 where T == Int {
  class Test {
    var test1: T {
      init(initialValue) { }
      set {}
      get { 0 }
    }

    var test2: U {
      init(initialValue) { }
      set {}
      get { fatalError() }
    }

    // CHECK-LABEL: sil hidden [ossa] @$s23assign_or_init_lowering2S2VAASiRszrlE4TestC1uAEySiq__Gq__tcfc : $@convention(method) <T, U where T == Int> (@in U, @owned S2<Int, U>.Test) -> @owned S2<Int, U>.Test {
    //
    // CHECK: [[TEST1_INIT_REF:%.*]] = function_ref @$s23assign_or_init_lowering2S2VAASiRszrlE4TestC5test1Sivi : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 == Int> (Int, @thick S2<Int, τ_0_1>.Test.Type) -> ()
    // CHECK-NEXT: [[SELF:%.*]] = value_metatype $@thick S2<Int, U>.Test.Type, {{.*}} : $S2<Int, U>.Test
    // CHECK-NEXT: [[TEST1_INIT_REF_WITH_SELF_APPLIED:%.*]] = partial_apply [callee_guaranteed] [on_stack] [[TEST1_INIT_REF]]<Int, U>([[SELF]]) : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 == Int> (Int, @thick S2<Int, τ_0_1>.Test.Type) -> ()
    // CHECK: assign_or_init [init] #S2.Test.test1, self {{.*}} : $S2<Int, U>.Test, value {{.*}} : $Int, init [[TEST1_INIT_REF_WITH_SELF_APPLIED]] : $@noescape @callee_guaranteed (Int) -> (), set {{.*}} : $@noescape @callee_guaranteed (Int) -> ()
    //
    // CHECK: [[TEST2_INIT_REF:%.*]] = function_ref @$s23assign_or_init_lowering2S2VAASiRszrlE4TestC5test2q_vi : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 == Int> (@in τ_0_1, @thick S2<Int, τ_0_1>.Test.Type) -> ()
    // CHECK-NEXT: [[SELF:%.*]] = value_metatype $@thick S2<Int, U>.Test.Type, {{.*}} : $S2<Int, U>.Test
    // CHECK-NEXT: [[TEST2_INIT_REF_WITH_SELF_APPLIED:%.*]] = partial_apply [callee_guaranteed] [on_stack] [[TEST2_INIT_REF]]<Int, U>([[SELF]]) : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 == Int> (@in τ_0_1, @thick S2<Int, τ_0_1>.Test.Type) -> ()
    // CHECK: assign_or_init [init] #S2.Test.test2, self {{.*}} : $S2<Int, U>.Test, value {{.*}} : $*U, init [[TEST2_INIT_REF_WITH_SELF_APPLIED]] : $@noescape @callee_guaranteed (@in U) -> (), set {{.*}} : $@noescape @callee_guaranteed (@in U) -> ()
    // CHECK: } // end sil function '$s23assign_or_init_lowering2S2VAASiRszrlE4TestC1uAEySiq__Gq__tcfc'
    init(u: U) {
      test1 = 0
      test2 = u
    }
  }
}
