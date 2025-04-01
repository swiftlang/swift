// RUN: %target-swift-emit-silgen -module-name Test %s -verify \
// RUN:   -enable-experimental-feature CustomAvailability \
// RUN:   -define-enabled-availability-domain EnabledDomain \
// RUN:   -define-disabled-availability-domain DisabledDomain \
// RUN:   -define-dynamic-availability-domain DynamicDomain \
// RUN:   | %FileCheck %s --check-prefixes=CHECK

// RUN: %target-swift-emit-silgen -module-name Test %s -verify \
// RUN:   -enable-experimental-feature CustomAvailability \
// RUN:   -define-enabled-availability-domain EnabledDomain \
// RUN:   -define-disabled-availability-domain DisabledDomain \
// RUN:   -define-dynamic-availability-domain DynamicDomain \
// RUN:   -unavailable-decl-optimization=stub \
// RUN:   | %FileCheck %s --check-prefixes=CHECK

// RUN: %target-swift-emit-silgen -module-name Test %s -verify \
// RUN:   -enable-experimental-feature CustomAvailability \
// RUN:   -define-enabled-availability-domain EnabledDomain \
// RUN:   -define-disabled-availability-domain DisabledDomain \
// RUN:   -define-dynamic-availability-domain DynamicDomain \
// RUN:   -unavailable-decl-optimization=complete \
// RUN:   | %FileCheck %s --check-prefixes=CHECK

// REQUIRES: swift_feature_CustomAvailability

// CHECK: s4Test15alwaysAvailableyyF
public func alwaysAvailable() { }

// CHECK: s4Test24availableInEnabledDomainyyF
@available(EnabledDomain)
public func availableInEnabledDomain() { }

// CHECK-NOT: s4Test26unavailableInEnabledDomainyyF
@available(EnabledDomain, unavailable)
public func unavailableInEnabledDomain() { }

// CHECK-NOT: s4Test25availableInDisabledDomainyyF
@available(DisabledDomain)
public func availableInDisabledDomain() { }

// CHECK: s4Test27unavailableInDisabledDomainyyF
@available(DisabledDomain, unavailable)
public func unavailableInDisabledDomain() { }

// CHECK: s4Test24availableInDynamicDomainyyF
@available(DynamicDomain)
public func availableInDynamicDomain() { }

// CHECK: s4Test26unavailableInDynamicDomainyyF
@available(DynamicDomain, unavailable)
public func unavailableInDynamicDomain() { }

// CHECK: s4Test25deprecatedInEnabledDomainyyF
@available(EnabledDomain, deprecated)
public func deprecatedInEnabledDomain() { }

// FIXME: [availability] This decl should be skipped.
// CHECK: s4Test26deprecatedInDisabledDomainyyF
@available(DisabledDomain, deprecated)
public func deprecatedInDisabledDomain() { }

// CHECK: s4Test25deprecatedInDynamicDomainyyF
@available(DynamicDomain, deprecated)
public func deprecatedInDynamicDomain() { }

// CHECK: s4Test22renamedInEnabledDomainyyF
@available(EnabledDomain, renamed: "availableInEnabledDomain")
public func renamedInEnabledDomain() { }

// CHECK-NOT: s4Test23renamedInDisabledDomainyyF
@available(DisabledDomain, renamed: "availableInDisabledDomain")
public func renamedInDisabledDomain() { }

// CHECK: s4Test22renamedInDynamicDomainyyF
@available(DynamicDomain, renamed: "availableInDynamicDomain")
public func renamedInDynamicDomain() { }

// CHECK-NOT: s4Test35availableInEnabledAndDisabledDomainyyF
@available(EnabledDomain)
@available(DisabledDomain)
public func availableInEnabledAndDisabledDomain() { }

// CHECK-NOT: s4Test35availableInDisabledAndEnabledDomainyyF
@available(DisabledDomain)
@available(EnabledDomain)
public func availableInDisabledAndEnabledDomain() { }

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

