// RUN: %target-swift-emit-silgen -module-name Test %s -verify \
// RUN:   -enable-experimental-feature CustomAvailability \
// RUN:   -define-enabled-availability-domain EnabledDomain \
// RUN:   -define-disabled-availability-domain DisabledDomain \
// RUN:   -define-dynamic-availability-domain DynamicDomain \
// RUN:   | %FileCheck %s

// REQUIRES: swift_feature_CustomAvailability

@available(EnabledDomain)
public func availableInEnabledDomain() { }

@available(EnabledDomain, unavailable)
public func unavailableInEnabledDomain() { }

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
