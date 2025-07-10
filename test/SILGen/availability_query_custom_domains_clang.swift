// RUN: %target-swift-emit-silgen -module-name Test %s -verify \
// RUN:   -enable-experimental-feature CustomAvailability \
// RUN:   -import-bridging-header %S/Inputs/AvailabilityDomains.h \
// RUN:   | %FileCheck %s

// REQUIRES: swift_feature_CustomAvailability

// CHECK-LABEL: sil{{.*}}$s4Test28testIfAvailableEnabledDomainyyF : $@convention(thin) () -> ()
public func testIfAvailableEnabledDomain() {
  // CHECK: bb0:
  // CHECK:   [[PRED:%.*]] = integer_literal $Builtin.Int1, -1
  // CHECK:   cond_br [[PRED]], [[TRUE_BB:bb[0-9]+]], [[FALSE_BB:bb[0-9]+]]

  // CHECK: [[TRUE_BB]]:
  // CHECK:   function_ref @available_in_enabled_domain

  // CHECK: [[FALSE_BB]]:
  // CHECK:   function_ref @unavailable_in_enabled_domain
  if #available(EnabledDomain) {
    available_in_enabled_domain()
  } else {
    unavailable_in_enabled_domain()
  }
}
// CHECK: end sil function '$s4Test28testIfAvailableEnabledDomainyyF'

// CHECK-LABEL: sil{{.*}}$s4Test30testIfUnavailableEnabledDomainyyF : $@convention(thin) () -> ()
public func testIfUnavailableEnabledDomain() {
  // CHECK: bb0:
  // CHECK:   [[PRED:%.*]] = integer_literal $Builtin.Int1, 0
  // CHECK:   cond_br [[PRED]], [[TRUE_BB:bb[0-9]+]], [[FALSE_BB:bb[0-9]+]]

  // CHECK: [[TRUE_BB]]:
  // CHECK:   function_ref @unavailable_in_enabled_domain

  // CHECK: [[FALSE_BB]]:
  // CHECK:   function_ref @available_in_enabled_domain
  if #unavailable(EnabledDomain) {
    unavailable_in_enabled_domain()
  } else {
    available_in_enabled_domain()
  }
}
// CHECK: end sil function '$s4Test30testIfUnavailableEnabledDomainyyF'

// CHECK-LABEL: sil{{.*}}$s4Test29testIfAvailableDisabledDomainyyF : $@convention(thin) () -> ()
public func testIfAvailableDisabledDomain() {
  // CHECK: bb0:
  // CHECK:   [[PRED:%.*]] = integer_literal $Builtin.Int1, 0
  // CHECK:   cond_br [[PRED]], [[TRUE_BB:bb[0-9]+]], [[FALSE_BB:bb[0-9]+]]

  // CHECK: [[TRUE_BB]]:
  // CHECK:   function_ref @available_in_disabled_domain

  // CHECK: [[FALSE_BB]]:
  // CHECK:   function_ref @unavailable_in_disabled_domain
  if #available(DisabledDomain) {
    available_in_disabled_domain()
  } else {
    unavailable_in_disabled_domain()
  }
}
// CHECK: end sil function '$s4Test29testIfAvailableDisabledDomainyyF'

// CHECK-LABEL: sil{{.*}}$s4Test31testIfUnavailableDisabledDomainyyF : $@convention(thin) () -> ()
public func testIfUnavailableDisabledDomain() {
  // CHECK: bb0:
  // CHECK:   [[PRED:%.*]] = integer_literal $Builtin.Int1, -1
  // CHECK:   cond_br [[PRED]], [[TRUE_BB:bb[0-9]+]], [[FALSE_BB:bb[0-9]+]]

  // CHECK: [[TRUE_BB]]:
  // CHECK:   function_ref @unavailable_in_disabled_domain

  // CHECK: [[FALSE_BB]]:
  // CHECK:   function_ref @available_in_disabled_domain
  if #unavailable(DisabledDomain) {
    unavailable_in_disabled_domain()
  } else {
    available_in_disabled_domain()
  }
}
// CHECK: end sil function '$s4Test31testIfUnavailableDisabledDomainyyF'

// CHECK-LABEL: sil{{.*}}$s4Test28testIfAvailableDynamicDomainyyF : $@convention(thin) () -> ()
public func testIfAvailableDynamicDomain() {
  // CHECK: bb0:
  // CHECK:   [[QUERY_FUNC:%.*]] = function_ref @$sSC33__swift_DynamicDomain_isAvailableBi1_yF : $@convention(thin) () -> Builtin.Int1
  // CHECK:   [[QUERY_RESULT:%.*]] = apply [[QUERY_FUNC]]() : $@convention(thin) () -> Builtin.Int1
  // CHECK:   cond_br [[QUERY_RESULT]], [[TRUE_BB:bb[0-9]+]], [[FALSE_BB:bb[0-9]+]]

  // CHECK: [[TRUE_BB]]:
  // CHECK:   function_ref @available_in_dynamic_domain

  // CHECK: [[FALSE_BB]]:
  // CHECK:   function_ref @unavailable_in_dynamic_domain
  if #available(DynamicDomain) {
    available_in_dynamic_domain()
  } else {
    unavailable_in_dynamic_domain()
  }
}
// CHECK: end sil function '$s4Test28testIfAvailableDynamicDomainyyF'

// CHECK-LABEL: sil non_abi [serialized] [ossa] @$sSC33__swift_DynamicDomain_isAvailableBi1_yF : $@convention(thin) () -> Builtin.Int1
// CHECK: bb0:
// CHECK:   [[QUERY_FUNC:%.*]] = function_ref @__DynamicDomain_isAvailable : $@convention(c) () -> Bool
// CHECK:   [[QUERY_RESULT:%.*]] = apply [[QUERY_FUNC]]() : $@convention(c) () -> Bool
// CHECK:   [[RESULT:%.*]] = struct_extract [[QUERY_RESULT]], #Bool._value
// CHECK:   return [[RESULT]]
// CHECK: end sil function '$sSC33__swift_DynamicDomain_isAvailableBi1_yF'

// CHECK-LABEL: sil{{.*}}$s4Test30testIfUnavailableDynamicDomainyyF : $@convention(thin) () -> ()
public func testIfUnavailableDynamicDomain() {
  // CHECK: bb0:
  // CHECK:   [[QUERY_FUNC:%.*]] = function_ref @$sSC33__swift_DynamicDomain_isAvailableBi1_yF : $@convention(thin) () -> Builtin.Int1
  // CHECK:   [[QUERY_RESULT:%.*]] = apply [[QUERY_FUNC]]() : $@convention(thin) () -> Builtin.Int1
  // CHECK:   [[MINUSONE:%.*]] = integer_literal $Builtin.Int1, -1
  // CHECK:   [[QUERY_INVERSION:%.*]] = builtin "xor_Int1"([[QUERY_RESULT]], [[MINUSONE]]) : $Builtin.Int1
  // CHECK:   cond_br [[QUERY_INVERSION]], [[TRUE_BB:bb[0-9]+]], [[FALSE_BB:bb[0-9]+]]

  // CHECK: [[TRUE_BB]]:
  // CHECK:   function_ref @unavailable_in_dynamic_domain

  // CHECK: [[FALSE_BB]]:
  // CHECK:   function_ref @available_in_dynamic_domain
  if #unavailable(DynamicDomain) {
    unavailable_in_dynamic_domain()
  } else {
    available_in_dynamic_domain()
  }
}
// CHECK: end sil function '$s4Test30testIfUnavailableDynamicDomainyyF'
