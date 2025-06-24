// RUN: %target-swift-emit-silgen -module-name Test %s -verify \
// RUN:   -enable-experimental-feature CustomAvailability \
// RUN:   -define-enabled-availability-domain EnabledDomain \
// RUN:   -define-disabled-availability-domain DisabledDomain \
// RUN:   -define-dynamic-availability-domain DynamicDomain \
// RUN:   | %FileCheck %s

// RUN: %target-swift-emit-silgen -module-name Test %s -verify \
// RUN:   -enable-experimental-feature CustomAvailability \
// RUN:   -define-enabled-availability-domain EnabledDomain \
// RUN:   -define-disabled-availability-domain DisabledDomain \
// RUN:   -define-dynamic-availability-domain DynamicDomain \
// RUN:   -unavailable-decl-optimization=stub \
// RUN:   | %FileCheck %s

// RUN: %target-swift-emit-silgen -module-name Test %s -verify \
// RUN:   -enable-experimental-feature CustomAvailability \
// RUN:   -define-enabled-availability-domain EnabledDomain \
// RUN:   -define-disabled-availability-domain DisabledDomain \
// RUN:   -define-dynamic-availability-domain DynamicDomain \
// RUN:   -unavailable-decl-optimization=complete \
// RUN:   | %FileCheck %s

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
