// RUN: %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values %s
// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-silgen %s -module-name test -swift-version 5 | %FileCheck %s
// REQUIRES: concurrency

@available(SwiftStdlib 5.1, *)
func f(isolatedTo actor: isolated (any Actor)?) async -> Int { 0 }

@available(SwiftStdlib 5.1, *)
actor A {
  let number: Int

  // CHECK-LABEL: sil hidden{{.*}}[ossa] @$s4test1ACACyYacfc : $@convention(method) @async (@sil_isolated @owned A) -> @owned A
  init() async {
    // First use of #isolation.
    // CHECK: [[ISOLATION_1:%.*]] = builtin "flowSensitiveSelfIsolation"<A>
    // CHECK: [[F_1:%.*]] = function_ref @$s4test1f10isolatedToSiScA_pSgYi_tYaF
    // CHECK-NEXT: [[F_RESULT:%.*]] = apply [[F_1]]([[ISOLATION_1]])

    // Assignment to "number" of the result.
    // CHECK: [[NUMBER:%.*]] = ref_element_addr {{%.*}} : $A, #A.number
    // CHECK: assign [[F_RESULT]] to [[NUMBER]]
    self.number = await f(isolatedTo: #isolation)

    // Second use of #isolation.
    // CHECK: [[ISOLATION_2:%.*]] = builtin "flowSensitiveSelfIsolation"<A>
    // CHECK: [[F_2:%.*]] = function_ref @$s4test1f10isolatedToSiScA_pSgYi_tYaF
    // CHECK-NEXT: apply [[F_2]]([[ISOLATION_2]])
    _ = await f(isolatedTo: #isolation)
  }
}

@available(SwiftStdlib 5.1, *)
func g(isolatedTo actor: isolated any Actor = #isolation) {}

@available(SwiftStdlib 5.1, *)
func h(isolatedTo actor: isolated B = #isolation) {}

// https://github.com/swiftlang/swift/issues/93800
@available(SwiftStdlib 5.1, *)
actor B {
  let number: Int

  // CHECK-LABEL: sil hidden{{.*}}[ossa] @$s4test1BCACyYacfc : $@convention(method) @async (@sil_isolated @owned B) -> @owned B
  init() async {
    number = 0

    // CHECK-NOT: builtin "flowSensitiveSelfIsolation"
    // CHECK: [[SELF_COPY:%.*]] = copy_value {{%.*}} : $B
    // CHECK-NEXT: [[ISOLATION:%.*]] = init_existential_ref [[SELF_COPY]] : $B : $B, $any Actor
    // CHECK-NOT: builtin "flowSensitiveSelfIsolation"
    // CHECK: [[G:%.*]] = function_ref @$s4test1g10isolatedToyScA_pYi_tF
    // CHECK-NEXT: apply [[G]]([[ISOLATION]])
    g()

    // CHECK-NOT: builtin "flowSensitiveSelfIsolation"
    // CHECK: [[SELF:%.*]] = copy_value {{%.*}} : $B
    // CHECK: [[H:%.*]] = function_ref @$s4test1h10isolatedToyAA1BCYi_tF
    // CHECK-NEXT: apply [[H]]([[SELF]])
    // CHECK: } // end sil function '$s4test1BCACyYacfc'
    h()
  }
}
