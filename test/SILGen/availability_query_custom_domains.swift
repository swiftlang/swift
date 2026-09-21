// RUN: %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values -module-name Test %s -verify -enable-experimental-feature CustomAvailability -define-enabled-availability-domain EnabledDomain -define-always-enabled-availability-domain AlwaysEnabledDomain -define-disabled-availability-domain DisabledDomain -define-dynamic-availability-domain DynamicDomain
// RUN: %target-swift-emit-silgen -module-name Test %s -verify \
// RUN:   -enable-experimental-feature CustomAvailability \
// RUN:   -define-enabled-availability-domain EnabledDomain \
// RUN:   -define-always-enabled-availability-domain AlwaysEnabledDomain \
// RUN:   -define-disabled-availability-domain DisabledDomain \
// RUN:   -define-dynamic-availability-domain DynamicDomain \
// RUN:   | %FileCheck %s

// REQUIRES: swift_feature_CustomAvailability

@available(EnabledDomain)
public func availableInEnabledDomain() { }

@available(EnabledDomain, unavailable)
public func unavailableInEnabledDomain() { }

@available(AlwaysEnabledDomain)
public func availableInAlwaysEnabledDomain() { }

@available(AlwaysEnabledDomain, unavailable)
public func unavailableInAlwaysEnabledDomain() { }

@available(DisabledDomain)
public func availableInDisabledDomain() { }

@available(DisabledDomain, unavailable)
public func unavailableInDisabledDomain() { }

@available(DynamicDomain)
public func availableInDynamicDomain() { }

@available(DynamicDomain, unavailable)
public func unavailableInDynamicDomain() { }

// CHECK-LABEL: sil{{.*}}$s4Test28testIfAvailableEnabledDomainyyF : $@convention(thin) () -> ()
public func testIfAvailableEnabledDomain() {
  // CHECK: bb0:
  // CHECK:   [[PRED:%.*]] = integer_literal $Builtin.Int1, -1
  // CHECK:   cond_br [[PRED]], [[TRUE_BB:bb[0-9]+]], [[FALSE_BB:bb[0-9]+]]

  // CHECK: [[TRUE_BB]]:
  // CHECK:   function_ref @$s4Test24availableInEnabledDomainyyF

  // CHECK: [[FALSE_BB]]:
  // CHECK:   function_ref @$s4Test26unavailableInEnabledDomainyyF
  if #available(EnabledDomain) {
    availableInEnabledDomain()
  } else {
    unavailableInEnabledDomain()
  }
}
// CHECK: end sil function '$s4Test28testIfAvailableEnabledDomainyyF'

// CHECK-LABEL: sil{{.*}}$s4Test30testIfUnavailableEnabledDomainyyF : $@convention(thin) () -> ()
public func testIfUnavailableEnabledDomain() {
  // CHECK: bb0:
  // CHECK:   [[PRED:%.*]] = integer_literal $Builtin.Int1, 0
  // CHECK:   cond_br [[PRED]], [[TRUE_BB:bb[0-9]+]], [[FALSE_BB:bb[0-9]+]]

  // CHECK: [[TRUE_BB]]:
  // CHECK:   function_ref @$s4Test26unavailableInEnabledDomainyyF

  // CHECK: [[FALSE_BB]]:
  // CHECK:   function_ref @$s4Test24availableInEnabledDomainyyF
  if #unavailable(EnabledDomain) {
    unavailableInEnabledDomain()
  } else {
    availableInEnabledDomain()
  }
}
// CHECK: end sil function '$s4Test30testIfUnavailableEnabledDomainyyF'

// CHECK-LABEL: sil{{.*}}$s4Test34testIfAvailableAlwaysEnabledDomainyyF : $@convention(thin) () -> ()
public func testIfAvailableAlwaysEnabledDomain() {
  // CHECK: bb0:
  // CHECK:   [[PRED:%.*]] = integer_literal $Builtin.Int1, -1
  // CHECK:   cond_br [[PRED]], [[TRUE_BB:bb[0-9]+]], [[FALSE_BB:bb[0-9]+]]

  // CHECK: [[TRUE_BB]]:
  // CHECK:   function_ref @$s4Test30availableInAlwaysEnabledDomainyyF

  // CHECK: [[FALSE_BB]]:
  // CHECK:   function_ref @$s4Test32unavailableInAlwaysEnabledDomainyyF
  if #available(AlwaysEnabledDomain) {
    availableInAlwaysEnabledDomain()
  } else {
    unavailableInAlwaysEnabledDomain()
  }
}
// CHECK: end sil function '$s4Test34testIfAvailableAlwaysEnabledDomainyyF'

// CHECK-LABEL: sil{{.*}}$s4Test36testIfUnavailableAlwaysEnabledDomainyyF : $@convention(thin) () -> ()
public func testIfUnavailableAlwaysEnabledDomain() {
  // CHECK: bb0:
  // CHECK:   [[PRED:%.*]] = integer_literal $Builtin.Int1, 0
  // CHECK:   cond_br [[PRED]], [[TRUE_BB:bb[0-9]+]], [[FALSE_BB:bb[0-9]+]]

  // CHECK: [[TRUE_BB]]:
  // CHECK:   function_ref @$s4Test32unavailableInAlwaysEnabledDomainyyF

  // CHECK: [[FALSE_BB]]:
  // CHECK:   function_ref @$s4Test30availableInAlwaysEnabledDomainyyF
  if #unavailable(AlwaysEnabledDomain) {
    unavailableInAlwaysEnabledDomain()
  } else {
    availableInAlwaysEnabledDomain()
  }
}
// CHECK: end sil function '$s4Test36testIfUnavailableAlwaysEnabledDomainyyF'

// CHECK-LABEL: sil{{.*}}$s4Test29testIfAvailableDisabledDomainyyF : $@convention(thin) () -> ()
public func testIfAvailableDisabledDomain() {
  // CHECK: bb0:
  // CHECK:   [[PRED:%.*]] = integer_literal $Builtin.Int1, 0
  // CHECK:   cond_br [[PRED]], [[TRUE_BB:bb[0-9]+]], [[FALSE_BB:bb[0-9]+]]

  // CHECK: [[TRUE_BB]]:
  // CHECK:   function_ref @$s4Test25availableInDisabledDomainyyF

  // CHECK: [[FALSE_BB]]:
  // CHECK:   function_ref @$s4Test27unavailableInDisabledDomainyyF
  if #available(DisabledDomain) {
    availableInDisabledDomain()
  } else {
    unavailableInDisabledDomain()
  }
}
// CHECK: end sil function '$s4Test29testIfAvailableDisabledDomainyyF'

// CHECK-LABEL: sil{{.*}}$s4Test31testIfUnavailableDisabledDomainyyF : $@convention(thin) () -> ()
public func testIfUnavailableDisabledDomain() {
  // CHECK: bb0:
  // CHECK:   [[PRED:%.*]] = integer_literal $Builtin.Int1, -1
  // CHECK:   cond_br [[PRED]], [[TRUE_BB:bb[0-9]+]], [[FALSE_BB:bb[0-9]+]]

  // CHECK: [[TRUE_BB]]:
  // CHECK:   function_ref @$s4Test27unavailableInDisabledDomainyyF

  // CHECK: [[FALSE_BB]]:
  // CHECK:   function_ref @$s4Test25availableInDisabledDomainyyF
  if #unavailable(DisabledDomain) {
    unavailableInDisabledDomain()
  } else {
    availableInDisabledDomain()
  }
}
// CHECK: end sil function '$s4Test31testIfUnavailableDisabledDomainyyF'

// CHECK-LABEL: sil{{.*}}$s4Test28testIfAvailableDynamicDomainyyF : $@convention(thin) () -> ()
public func testIfAvailableDynamicDomain() {
  // FIXME: [availability] Call dynamic domain predicate function
  // CHECK: bb0:
  // CHECK:   [[PRED:%.*]] = integer_literal $Builtin.Int1, -1
  // CHECK:   cond_br [[PRED]], [[TRUE_BB:bb[0-9]+]], [[FALSE_BB:bb[0-9]+]]

  // CHECK: [[TRUE_BB]]:
  // CHECK:   function_ref @$s4Test24availableInDynamicDomainyyF

  // CHECK: [[FALSE_BB]]:
  // CHECK:   function_ref @$s4Test26unavailableInDynamicDomainyyF
  if #available(DynamicDomain) {
    availableInDynamicDomain()
  } else {
    unavailableInDynamicDomain()
  }
}
// CHECK: end sil function '$s4Test28testIfAvailableDynamicDomainyyF'

// CHECK-LABEL: sil{{.*}}$s4Test30testIfUnavailableDynamicDomainyyF : $@convention(thin) () -> ()
public func testIfUnavailableDynamicDomain() {
  // FIXME: [availability] Call dynamic domain predicate function
  // CHECK: bb0:
  // CHECK:   [[PRED:%.*]] = integer_literal $Builtin.Int1, 0
  // CHECK:   cond_br [[PRED]], [[TRUE_BB:bb[0-9]+]], [[FALSE_BB:bb[0-9]+]]

  // CHECK: [[TRUE_BB]]:
  // CHECK:   function_ref @$s4Test26unavailableInDynamicDomainyyF

  // CHECK: [[FALSE_BB]]:
  // CHECK:   function_ref @$s4Test24availableInDynamicDomainyyF
  if #unavailable(DynamicDomain) {
    unavailableInDynamicDomain()
  } else {
    availableInDynamicDomain()
  }
}
// CHECK: end sil function '$s4Test30testIfUnavailableDynamicDomainyyF'

// CHECK-LABEL: sil{{.*}}testIfAvailableEnabledDomainAlways{{[a-zA-Z0-9]*}}yyF : $@convention(thin) () -> ()
public func testIfAvailableEnabledDomainAlwaysEnabledDomain() {
  // CHECK: bb0:
  // CHECK:   [[PRED0:%.*]] = integer_literal $Builtin.Int1, -1
  // CHECK:   cond_br [[PRED0]]

  // CHECK:   [[PRED1:%.*]] = integer_literal $Builtin.Int1, -1
  // CHECK:   cond_br [[PRED1]]

  // CHECK:   function_ref @$s4Test24availableInEnabledDomainyyF
  if #available(EnabledDomain), #available(AlwaysEnabledDomain) {
    availableInEnabledDomain()
  } else {
  }
}
// CHECK: end sil function '{{.*}}testIfAvailableEnabledDomainAlways{{[a-zA-Z0-9]*}}yyF'

// CHECK-LABEL: sil{{.*}}testIfAvailableEnabledDomainDisabled{{[a-zA-Z0-9]*}}yyF : $@convention(thin) () -> ()
public func testIfAvailableEnabledDomainDisabledDomain() {
  // CHECK: bb0:
  // CHECK:   [[PRED0:%.*]] = integer_literal $Builtin.Int1, -1
  // CHECK:   cond_br [[PRED0]], [[TRUE_BB0:bb[0-9]+]], [[FALSE_BB0:bb[0-9]+]]

  // CHECK: [[TRUE_BB0]]:
  // CHECK:   [[PRED1:%.*]] = integer_literal $Builtin.Int1, 0
  // CHECK:   cond_br [[PRED1]], [[TRUE_BB1:bb[0-9]+]], [[FALSE_BB1:bb[0-9]+]]

  // CHECK: [[TRUE_BB1]]:
  // CHECK:   function_ref @$s4Test25availableInDisabledDomainyyF
  // CHECK:   br [[CONT_BB:bb[0-9]+]]

  // CHECK: [[FALSE_BB1]]:
  // CHECK-NEXT:   br [[ELSE_BB:bb[0-9]+]]

  // CHECK: [[FALSE_BB0]]:
  // CHECK-NEXT:   br [[ELSE_BB]]

  // CHECK: [[ELSE_BB]]:
  // CHECK:   function_ref @$s4Test30availableInAlwaysEnabledDomainyyF
  // CHECK:   br [[CONT_BB]]
  if #available(EnabledDomain), #available(DisabledDomain) {
    availableInDisabledDomain()
  } else {
    availableInAlwaysEnabledDomain()
  }
}
// CHECK: end sil function '{{.*}}testIfAvailableEnabledDomainDisabled{{[a-zA-Z0-9]*}}yyF'

// CHECK-LABEL: sil{{.*}}testIfAvailableEnabledDomainDynamic{{[a-zA-Z0-9]*}}yyF : $@convention(thin) () -> ()
public func testIfAvailableEnabledDomainDynamicDomain() {
  // FIXME: [availability] Call dynamic domain predicate function
  // CHECK: bb0:
  // CHECK:   [[PRED0:%.*]] = integer_literal $Builtin.Int1, -1
  // CHECK:   cond_br [[PRED0]], [[TRUE_BB0:bb[0-9]+]], [[FALSE_BB0:bb[0-9]+]]

  // CHECK: [[TRUE_BB0]]:
  // CHECK:   [[PRED1:%.*]] = integer_literal $Builtin.Int1, -1
  // CHECK:   cond_br [[PRED1]], [[TRUE_BB1:bb[0-9]+]], [[FALSE_BB1:bb[0-9]+]]

  // CHECK: [[TRUE_BB1]]:
  // CHECK:   function_ref @$s4Test24availableInDynamicDomainyyF
  // CHECK:   br [[CONT_BB:bb[0-9]+]]

  // CHECK: [[FALSE_BB1]]:
  // CHECK-NEXT:   br [[ELSE_BB:bb[0-9]+]]

  // CHECK: [[FALSE_BB0]]:
  // CHECK-NEXT:   br [[ELSE_BB]]

  // CHECK: [[ELSE_BB]]:
  // CHECK:   function_ref @$s4Test30availableInAlwaysEnabledDomainyyF
  // CHECK:   br [[CONT_BB]]
  if #available(EnabledDomain), #available(DynamicDomain) {
    availableInDynamicDomain()
  } else {
    availableInAlwaysEnabledDomain()
  }
}
// CHECK: end sil function '{{.*}}testIfAvailableEnabledDomainDynamic{{[a-zA-Z0-9]*}}yyF'

// CHECK-LABEL: sil{{.*}}$s4Test31testGuardAvailableEnabledDomainyyF : $@convention(thin) () -> ()
public func testGuardAvailableEnabledDomain() {
  // CHECK: bb0:
  // CHECK:   [[PRED:%.*]] = integer_literal $Builtin.Int1, -1
  // CHECK:   cond_br [[PRED]], [[TRUE_BB:bb[0-9]+]], [[FALSE_BB:bb[0-9]+]]

  // CHECK: [[TRUE_BB]]:
  // CHECK:   function_ref @$s4Test24availableInEnabledDomainyyF

  // CHECK: [[FALSE_BB]]:
  // CHECK:   function_ref @$s4Test26unavailableInEnabledDomainyyF
  guard #available(EnabledDomain) else {
    unavailableInEnabledDomain()
    return
  }
  availableInEnabledDomain()
}
// CHECK: end sil function '$s4Test31testGuardAvailableEnabledDomainyyF'

// CHECK-LABEL: sil{{.*}}$s4Test33testGuardUnavailableEnabledDomainyyF : $@convention(thin) () -> ()
public func testGuardUnavailableEnabledDomain() {
  // CHECK: bb0:
  // CHECK:   [[PRED:%.*]] = integer_literal $Builtin.Int1, 0
  // CHECK:   cond_br [[PRED]], [[TRUE_BB:bb[0-9]+]], [[FALSE_BB:bb[0-9]+]]

  // CHECK: [[TRUE_BB]]:
  // CHECK:   function_ref @$s4Test26unavailableInEnabledDomainyyF

  // CHECK: [[FALSE_BB]]:
  // CHECK:   function_ref @$s4Test24availableInEnabledDomainyyF
  guard #unavailable(EnabledDomain) else {
    availableInEnabledDomain()
    return
  }
  unavailableInEnabledDomain()
}
// CHECK: end sil function '$s4Test33testGuardUnavailableEnabledDomainyyF'

// CHECK-LABEL: sil{{.*}}$s4Test32testGuardAvailableDisabledDomainyyF : $@convention(thin) () -> ()
public func testGuardAvailableDisabledDomain() {
  // CHECK: bb0:
  // CHECK:   [[PRED:%.*]] = integer_literal $Builtin.Int1, 0
  // CHECK:   cond_br [[PRED]], [[TRUE_BB:bb[0-9]+]], [[FALSE_BB:bb[0-9]+]]

  // CHECK: [[TRUE_BB]]:
  // CHECK:   function_ref @$s4Test25availableInDisabledDomainyyF

  // CHECK: [[FALSE_BB]]:
  // CHECK:   function_ref @$s4Test27unavailableInDisabledDomainyyF
  guard #available(DisabledDomain) else {
    unavailableInDisabledDomain()
    return
  }
  availableInDisabledDomain()
}
// CHECK: end sil function '$s4Test32testGuardAvailableDisabledDomainyyF'

// CHECK-LABEL: sil{{.*}}$s4Test34testGuardUnavailableDisabledDomainyyF : $@convention(thin) () -> ()
public func testGuardUnavailableDisabledDomain() {
  // CHECK: bb0:
  // CHECK:   [[PRED:%.*]] = integer_literal $Builtin.Int1, -1
  // CHECK:   cond_br [[PRED]], [[TRUE_BB:bb[0-9]+]], [[FALSE_BB:bb[0-9]+]]

  // CHECK: [[TRUE_BB]]:
  // CHECK:   function_ref @$s4Test27unavailableInDisabledDomainyyF

  // CHECK: [[FALSE_BB]]:
  // CHECK:   function_ref @$s4Test25availableInDisabledDomainyyF
  guard #unavailable(DisabledDomain) else {
    availableInDisabledDomain()
    return
  }
  unavailableInDisabledDomain()
}
// CHECK: end sil function '$s4Test34testGuardUnavailableDisabledDomainyyF'

// CHECK-LABEL: sil{{.*}}$s4Test31testGuardAvailableDynamicDomainyyF : $@convention(thin) () -> ()
public func testGuardAvailableDynamicDomain() {
  // FIXME: [availability] Call dynamic domain predicate function
  // CHECK: bb0:
  // CHECK:   [[PRED:%.*]] = integer_literal $Builtin.Int1, -1
  // CHECK:   cond_br [[PRED]], [[TRUE_BB:bb[0-9]+]], [[FALSE_BB:bb[0-9]+]]

  // CHECK: [[TRUE_BB]]:
  // CHECK:   function_ref @$s4Test24availableInDynamicDomainyyF

  // CHECK: [[FALSE_BB]]:
  // CHECK:   function_ref @$s4Test26unavailableInDynamicDomainyyF
  guard #available(DynamicDomain) else {
    unavailableInDynamicDomain()
    return
  }
  availableInDynamicDomain()
}
// CHECK: end sil function '$s4Test31testGuardAvailableDynamicDomainyyF'

// CHECK-LABEL: sil{{.*}}testGuardAvailableEnabledDomainDisabled{{[a-zA-Z0-9]*}}yyF : $@convention(thin) () -> ()
public func testGuardAvailableEnabledDomainDisabledDomain() {
  // CHECK: bb0:
  // CHECK:   [[PRED0:%.*]] = integer_literal $Builtin.Int1, -1
  // CHECK:   cond_br [[PRED0]], [[TRUE_BB0:bb[0-9]+]], [[FALSE_BB0:bb[0-9]+]]

  // CHECK: [[TRUE_BB0]]:
  // CHECK:   [[PRED1:%.*]] = integer_literal $Builtin.Int1, 0
  // CHECK:   cond_br [[PRED1]], [[TRUE_BB1:bb[0-9]+]], [[FALSE_BB1:bb[0-9]+]]

  // CHECK: [[TRUE_BB1]]:
  // CHECK:   function_ref @$s4Test25availableInDisabledDomainyyF
  // CHECK:   br [[CONT_BB:bb[0-9]+]]

  // CHECK: [[FALSE_BB1]]:
  // CHECK-NEXT:   br [[ELSE_BB:bb[0-9]+]]

  // CHECK: [[FALSE_BB0]]:
  // CHECK-NEXT:   br [[ELSE_BB]]

  // CHECK: [[ELSE_BB]]:
  // CHECK:   function_ref @$s4Test30availableInAlwaysEnabledDomainyyF
  // CHECK:   br [[CONT_BB]]
  guard #available(EnabledDomain), #available(DisabledDomain) else {
    availableInAlwaysEnabledDomain()
    return
  }
  availableInDisabledDomain()
}
// CHECK: end sil function '{{.*}}testGuardAvailableEnabledDomainDisabled{{[a-zA-Z0-9]*}}yyF'

// CHECK-LABEL: sil{{.*}}$s4Test31testWhileAvailableEnabledDomainyyF : $@convention(thin) () -> ()
public func testWhileAvailableEnabledDomain() {
  // CHECK: bb0:
  // CHECK:   br [[LOOP_BB:bb[0-9]+]]

  // CHECK: [[LOOP_BB]]:
  // CHECK:   [[PRED:%.*]] = integer_literal $Builtin.Int1, -1
  // CHECK:   cond_br [[PRED]], [[BODY_BB:bb[0-9]+]], [[EXIT_BB:bb[0-9]+]]

  // CHECK: [[BODY_BB]]:
  // CHECK:   function_ref @$s4Test24availableInEnabledDomainyyF
  // CHECK:   br [[LOOP_BB]]

  // CHECK: [[EXIT_BB]]:
  // CHECK-NEXT:   tuple
  // CHECK-NEXT:   return
  while #available(EnabledDomain) {
    availableInEnabledDomain()
  }
}
// CHECK: end sil function '$s4Test31testWhileAvailableEnabledDomainyyF'

// CHECK-LABEL: sil{{.*}}$s4Test33testWhileUnavailableEnabledDomainyyF : $@convention(thin) () -> ()
public func testWhileUnavailableEnabledDomain() {
  // CHECK: bb0:
  // CHECK:   br [[LOOP_BB:bb[0-9]+]]

  // CHECK: [[LOOP_BB]]:
  // CHECK:   [[PRED:%.*]] = integer_literal $Builtin.Int1, 0
  // CHECK:   cond_br [[PRED]], [[BODY_BB:bb[0-9]+]], [[EXIT_BB:bb[0-9]+]]

  // CHECK: [[BODY_BB]]:
  // CHECK:   function_ref @$s4Test26unavailableInEnabledDomainyyF
  // CHECK:   br [[LOOP_BB]]

  // CHECK: [[EXIT_BB]]:
  // CHECK-NEXT:   tuple
  // CHECK-NEXT:   return
  while #unavailable(EnabledDomain) {
    unavailableInEnabledDomain()
  }
}
// CHECK: end sil function '$s4Test33testWhileUnavailableEnabledDomainyyF'

// CHECK-LABEL: sil{{.*}}$s4Test32testWhileAvailableDisabledDomainyyF : $@convention(thin) () -> ()
public func testWhileAvailableDisabledDomain() {
  // CHECK: bb0:
  // CHECK:   br [[LOOP_BB:bb[0-9]+]]

  // CHECK: [[LOOP_BB]]:
  // CHECK:   [[PRED:%.*]] = integer_literal $Builtin.Int1, 0
  // CHECK:   cond_br [[PRED]], [[BODY_BB:bb[0-9]+]], [[EXIT_BB:bb[0-9]+]]

  // CHECK: [[BODY_BB]]:
  // CHECK:   function_ref @$s4Test25availableInDisabledDomainyyF
  // CHECK:   br [[LOOP_BB]]

  // CHECK: [[EXIT_BB]]:
  // CHECK-NEXT:   tuple
  // CHECK-NEXT:   return
  while #available(DisabledDomain) {
    availableInDisabledDomain()
  }
}
// CHECK: end sil function '$s4Test32testWhileAvailableDisabledDomainyyF'

// CHECK-LABEL: sil{{.*}}$s4Test31testWhileAvailableDynamicDomainyyF : $@convention(thin) () -> ()
public func testWhileAvailableDynamicDomain() {
  // FIXME: [availability] Call dynamic domain predicate function
  // CHECK: bb0:
  // CHECK:   br [[LOOP_BB:bb[0-9]+]]

  // CHECK: [[LOOP_BB]]:
  // CHECK:   [[PRED:%.*]] = integer_literal $Builtin.Int1, -1
  // CHECK:   cond_br [[PRED]], [[BODY_BB:bb[0-9]+]], [[EXIT_BB:bb[0-9]+]]

  // CHECK: [[BODY_BB]]:
  // CHECK:   function_ref @$s4Test24availableInDynamicDomainyyF
  // CHECK:   br [[LOOP_BB]]

  // CHECK: [[EXIT_BB]]:
  // CHECK-NEXT:   tuple
  // CHECK-NEXT:   return
  while #available(DynamicDomain) {
    availableInDynamicDomain()
  }
}
// CHECK: end sil function '$s4Test31testWhileAvailableDynamicDomainyyF'

// CHECK-LABEL: sil{{.*}}$s4Test40testIfAvailableDisabledDomainNestedDeclsyyF : $@convention(thin) () -> ()
public func testIfAvailableDisabledDomainNestedDecls() {
  // CHECK: bb0:
  // CHECK:   [[PRED:%.*]] = integer_literal $Builtin.Int1, 0
  // CHECK:   cond_br [[PRED]], [[TRUE_BB:bb[0-9]+]], [[FALSE_BB:bb[0-9]+]]

  // CHECK: [[TRUE_BB]]:
  // CHECK:   function_ref @$s4Test40testIfAvailableDisabledDomainNestedDeclsyyFyycfU_
  // CHECK:   function_ref @$s4Test40testIfAvailableDisabledDomainNestedDeclsyyF10nestedFuncL_yyF
  // CHECK:   function_ref @$s4Test40testIfAvailableDisabledDomainNestedDeclsyyF0G6StructL_V1myyF
  // CHECK:   br [[CONT_BB:bb[0-9]+]]

  // CHECK: [[FALSE_BB]]:
  // CHECK-NEXT:   br [[CONT_BB]]
  if #available(DisabledDomain) {
    func nestedFunc() { }
    let nestedClosure = { }
    struct NestedStruct {
      func m() { }
    }
    nestedFunc()
    nestedClosure()
    NestedStruct().m()
  }
}
// CHECK: end sil function '$s4Test40testIfAvailableDisabledDomainNestedDeclsyyF'

// The closure is emitted (but will be removed by mandatory optimization).
// CHECK: closure #1 in testIfAvailableDisabledDomainNestedDecls()
// CHECK-LABEL: sil private{{.*}}$s4Test40testIfAvailableDisabledDomainNestedDeclsyyFyycfU_ : $@convention(thin) () -> () {

// The nested function and the nested struct's members are emitted as
// declarations only; a trailing '{' would indicate that a body was emitted.
// CHECK-LABEL: sil{{.*}}$s4Test40testIfAvailableDisabledDomainNestedDeclsyyF10nestedFuncL_yyF : $@convention(thin) () -> (){{$}}
// CHECK-LABEL: sil{{.*}}$s4Test40testIfAvailableDisabledDomainNestedDeclsyyF0G6StructL_VADycfC : $@convention(method) (@thin NestedStruct.Type) -> NestedStruct{{$}}
// CHECK-LABEL: sil{{.*}}$s4Test40testIfAvailableDisabledDomainNestedDeclsyyF0G6StructL_V1myyF : $@convention(method) (NestedStruct) -> (){{$}}

// CHECK-LABEL: sil{{.*}}$s4Test43testIfAvailableEnabledDomainElseNestedDeclsyyF : $@convention(thin) () -> ()
public func testIfAvailableEnabledDomainElseNestedDecls() {
  // CHECK: bb0:
  // CHECK:   [[PRED:%.*]] = integer_literal $Builtin.Int1, -1
  // CHECK:   cond_br [[PRED]], [[TRUE_BB:bb[0-9]+]], [[FALSE_BB:bb[0-9]+]]

  // CHECK: [[TRUE_BB]]:
  // CHECK-NEXT:   br [[CONT_BB:bb[0-9]+]]

  // CHECK: [[FALSE_BB]]:
  // CHECK:   function_ref @$s4Test43testIfAvailableEnabledDomainElseNestedDeclsyyFyycfU_
  // CHECK:   function_ref @$s4Test43testIfAvailableEnabledDomainElseNestedDeclsyyF10nestedFuncL_yyF
  // CHECK:   function_ref @$s4Test43testIfAvailableEnabledDomainElseNestedDeclsyyF0H6StructL_V1myyF
  // CHECK:   br [[CONT_BB]]
  if #available(EnabledDomain) {
  } else {
    func nestedFunc() { }
    let nestedClosure = { }
    struct NestedStruct {
      func m() { }
    }
    nestedFunc()
    nestedClosure()
    NestedStruct().m()
  }
}
// CHECK: end sil function '$s4Test43testIfAvailableEnabledDomainElseNestedDeclsyyF'

// The closure is emitted (but will be removed by mandatory optimization).
// CHECK: closure #1 in testIfAvailableEnabledDomainElseNestedDecls()
// CHECK-LABEL: sil private{{.*}}$s4Test43testIfAvailableEnabledDomainElseNestedDeclsyyFyycfU_ : $@convention(thin) () -> () {

// CHECK-LABEL: sil{{.*}}$s4Test43testIfAvailableEnabledDomainElseNestedDeclsyyF10nestedFuncL_yyF : $@convention(thin) () -> (){{$}}
// CHECK-LABEL: sil{{.*}}$s4Test43testIfAvailableEnabledDomainElseNestedDeclsyyF0H6StructL_VADycfC : $@convention(method) (@thin NestedStruct.Type) -> NestedStruct{{$}}
// CHECK-LABEL: sil{{.*}}$s4Test43testIfAvailableEnabledDomainElseNestedDeclsyyF0H6StructL_V1myyF : $@convention(method) (NestedStruct) -> (){{$}}

// CHECK-LABEL: sil{{.*}}$s4Test43testGuardAvailableDisabledDomainNestedDeclsyyF : $@convention(thin) () -> ()
public func testGuardAvailableDisabledDomainNestedDecls() {
  // CHECK: bb0:
  // CHECK:   [[PRED:%.*]] = integer_literal $Builtin.Int1, 0
  // CHECK:   cond_br [[PRED]], [[TRUE_BB:bb[0-9]+]], [[FALSE_BB:bb[0-9]+]]

  // CHECK: [[TRUE_BB]]:
  // CHECK:   function_ref @$s4Test43testGuardAvailableDisabledDomainNestedDeclsyyFyycfU_
  // CHECK:   function_ref @$s4Test43testGuardAvailableDisabledDomainNestedDeclsyyF10nestedFuncL_yyF
  // CHECK:   function_ref @$s4Test43testGuardAvailableDisabledDomainNestedDeclsyyF0G6StructL_V1myyF
  // CHECK:   br [[CONT_BB:bb[0-9]+]]

  // CHECK: [[FALSE_BB]]:
  // CHECK-NEXT:   br [[CONT_BB]]
  guard #available(DisabledDomain) else { return }
  func nestedFunc() { }
  let nestedClosure = { }
  struct NestedStruct {
    func m() { }
  }
  nestedFunc()
  nestedClosure()
  NestedStruct().m()
}
// CHECK: end sil function '$s4Test43testGuardAvailableDisabledDomainNestedDeclsyyF'

// The closure is emitted (but will be removed by mandatory optimization).
// CHECK: closure #1 in testGuardAvailableDisabledDomainNestedDecls()
// CHECK-LABEL: sil private{{.*}}$s4Test43testGuardAvailableDisabledDomainNestedDeclsyyFyycfU_ : $@convention(thin) () -> () {

// CHECK-LABEL: sil{{.*}}$s4Test43testGuardAvailableDisabledDomainNestedDeclsyyF10nestedFuncL_yyF : $@convention(thin) () -> (){{$}}
// CHECK-LABEL: sil{{.*}}$s4Test43testGuardAvailableDisabledDomainNestedDeclsyyF0G6StructL_VADycfC : $@convention(method) (@thin NestedStruct.Type) -> NestedStruct{{$}}
// CHECK-LABEL: sil{{.*}}$s4Test43testGuardAvailableDisabledDomainNestedDeclsyyF0G6StructL_V1myyF : $@convention(method) (NestedStruct) -> (){{$}}

// CHECK-LABEL: sil{{.*}}$s4Test42testGuardAvailableEnabledDomainNestedDeclsyyF : $@convention(thin) () -> ()
public func testGuardAvailableEnabledDomainNestedDecls() {
  // CHECK: bb0:
  // CHECK:   [[PRED:%.*]] = integer_literal $Builtin.Int1, -1
  // CHECK:   cond_br [[PRED]], [[TRUE_BB:bb[0-9]+]], [[FALSE_BB:bb[0-9]+]]

  // CHECK: [[TRUE_BB]]:
  // CHECK:   function_ref @$s4Test24availableInEnabledDomainyyF

  // CHECK: [[FALSE_BB]]:
  // CHECK:   function_ref @$s4Test42testGuardAvailableEnabledDomainNestedDeclsyyFyycfU_
  // CHECK:   function_ref @$s4Test42testGuardAvailableEnabledDomainNestedDeclsyyF10nestedFuncL_yyF
  // CHECK:   function_ref @$s4Test42testGuardAvailableEnabledDomainNestedDeclsyyF0G6StructL_V1myyF
  guard #available(EnabledDomain) else {
    func nestedFunc() { }
    let nestedClosure = { }
    struct NestedStruct {
      func m() { }
    }
    nestedFunc()
    nestedClosure()
    NestedStruct().m()
    return
  }
  availableInEnabledDomain()
}
// CHECK: end sil function '$s4Test42testGuardAvailableEnabledDomainNestedDeclsyyF'

// The closure is emitted (but will be removed by mandatory optimization).
// CHECK: closure #1 in testGuardAvailableEnabledDomainNestedDecls()
// CHECK-LABEL: sil private{{.*}}$s4Test42testGuardAvailableEnabledDomainNestedDeclsyyFyycfU_ : $@convention(thin) () -> () {

// CHECK-LABEL: sil{{.*}}$s4Test42testGuardAvailableEnabledDomainNestedDeclsyyF10nestedFuncL_yyF : $@convention(thin) () -> (){{$}}
// CHECK-LABEL: sil{{.*}}$s4Test42testGuardAvailableEnabledDomainNestedDeclsyyF0G6StructL_VADycfC : $@convention(method) (@thin NestedStruct.Type) -> NestedStruct{{$}}
// CHECK-LABEL: sil{{.*}}$s4Test42testGuardAvailableEnabledDomainNestedDeclsyyF0G6StructL_V1myyF : $@convention(method) (NestedStruct) -> (){{$}}

// CHECK-LABEL: sil{{.*}}$s4Test43testWhileAvailableDisabledDomainNestedDeclsyyF : $@convention(thin) () -> ()
public func testWhileAvailableDisabledDomainNestedDecls() {
  // CHECK: bb0:
  // CHECK:   br [[LOOP_BB:bb[0-9]+]]

  // CHECK: [[LOOP_BB]]:
  // CHECK:   [[PRED:%.*]] = integer_literal $Builtin.Int1, 0
  // CHECK:   cond_br [[PRED]], [[BODY_BB:bb[0-9]+]], [[EXIT_BB:bb[0-9]+]]

  // CHECK: [[BODY_BB]]:
  // CHECK:   function_ref @$s4Test43testWhileAvailableDisabledDomainNestedDeclsyyFyycfU_
  // CHECK:   function_ref @$s4Test43testWhileAvailableDisabledDomainNestedDeclsyyF10nestedFuncL_yyF
  // CHECK:   function_ref @$s4Test43testWhileAvailableDisabledDomainNestedDeclsyyF0G6StructL_V1myyF
  // CHECK:   br [[LOOP_BB]]
  while #available(DisabledDomain) {
    func nestedFunc() { }
    let nestedClosure = { }
    struct NestedStruct {
      func m() { }
    }
    nestedFunc()
    nestedClosure()
    NestedStruct().m()
  }
}
// CHECK: end sil function '$s4Test43testWhileAvailableDisabledDomainNestedDeclsyyF'

// The closure is emitted (but will be removed by mandatory optimization).
// CHECK: closure #1 in testWhileAvailableDisabledDomainNestedDecls()
// CHECK-LABEL: sil private{{.*}}$s4Test43testWhileAvailableDisabledDomainNestedDeclsyyFyycfU_ : $@convention(thin) () -> () {

// CHECK-LABEL: sil{{.*}}$s4Test43testWhileAvailableDisabledDomainNestedDeclsyyF10nestedFuncL_yyF : $@convention(thin) () -> (){{$}}
// CHECK-LABEL: sil{{.*}}$s4Test43testWhileAvailableDisabledDomainNestedDeclsyyF0G6StructL_VADycfC : $@convention(method) (@thin NestedStruct.Type) -> NestedStruct{{$}}
// CHECK-LABEL: sil{{.*}}$s4Test43testWhileAvailableDisabledDomainNestedDeclsyyF0G6StructL_V1myyF : $@convention(method) (NestedStruct) -> (){{$}}
