// RUN: %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values -module-name Test %s -verify -enable-experimental-feature CustomAvailability -define-enabled-availability-domain EnabledDomain -define-always-enabled-availability-domain AlwaysEnabledDomain -define-disabled-availability-domain DisabledDomain -define-dynamic-availability-domain DynamicDomain
// RUN: %target-swift-emit-silgen -module-name Test %s -verify \
// RUN:   -enable-experimental-feature CustomAvailability \
// RUN:   -define-enabled-availability-domain EnabledDomain \
// RUN:   -define-always-enabled-availability-domain AlwaysEnabledDomain \
// RUN:   -define-disabled-availability-domain DisabledDomain \
// RUN:   -define-dynamic-availability-domain DynamicDomain \
// RUN:   | %FileCheck %s --check-prefixes=CHECK,CHECK-NOOPT

// RUN: %target-swift-emit-silgen -module-name Test %s -verify \
// RUN:   -enable-experimental-feature CustomAvailability \
// RUN:   -define-enabled-availability-domain EnabledDomain \
// RUN:   -define-always-enabled-availability-domain AlwaysEnabledDomain \
// RUN:   -define-disabled-availability-domain DisabledDomain \
// RUN:   -define-dynamic-availability-domain DynamicDomain \
// RUN:   -unavailable-decl-optimization=complete \
// RUN:   | %FileCheck %s --check-prefixes=CHECK,CHECK-OPT

// REQUIRES: swift_feature_CustomAvailability

// CHECK: s4Test15alwaysAvailableyyF
public func alwaysAvailable() { }

// CHECK: s4Test24availableInEnabledDomainyyF
@available(EnabledDomain)
public func availableInEnabledDomain() { }

// CHECK-NOT: s4Test26unavailableInEnabledDomainyyF
@available(EnabledDomain, unavailable)
public func unavailableInEnabledDomain() { }

// CHECK: s4Test30availableInAlwaysEnabledDomainyyF
@available(AlwaysEnabledDomain)
public func availableInAlwaysEnabledDomain() { }

// CHECK-NOT: s4Test32unavailableInAlwaysEnabledDomainyyF
@available(AlwaysEnabledDomain, unavailable)
public func unavailableInAlwaysEnabledDomain() { }

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

// CHECK-NOOPT: s4Test49availableInEnabledDomainAndUnavailableUniversallyyyF
// CHECK-OPT-NOT: s4Test49availableInEnabledDomainAndUnavailableUniversallyyyF
@available(*, unavailable)
@available(EnabledDomain)
public func availableInEnabledDomainAndUnavailableUniversally() { }

// CHECK-NOT: s4Test40unavailableInEnabledDomainAndUniversallyyyF
@available(*, unavailable)
@available(EnabledDomain, unavailable)
public func unavailableInEnabledDomainAndUniversally() { }

// CHECK-NOT: s4Test50availableInDisabledDomainAndUnavailableUniversallyyyF
@available(*, unavailable)
@available(DisabledDomain)
public func availableInDisabledDomainAndUnavailableUniversally() { }

// CHECK-NOOPT: s4Test41unavailableInDisabledDomainAndUniversallyyyF
// CHECK-OPT-NOT: s4Test41unavailableInDisabledDomainAndUniversallyyyF
@available(*, unavailable)
@available(DisabledDomain, unavailable)
public func unavailableInDisabledDomainAndUniversally() { }

public struct S {
  // CHECK: s4Test1SV20disabledDomainSetterSivg
  // CHECK-NOT: s4Test1SV20disabledDomainSetterSiv{{s|M}}
  public var disabledDomainSetter: Int {
    get { 0 }
    @available(DisabledDomain)
    set { }
  }

  // CHECK: s4Test1SV19enabledDomainSetterSivg
  // CHECK: s4Test1SV19enabledDomainSetterSivs
  // CHECK: s4Test1SV19enabledDomainSetterSivM
  public var enabledDomainSetter: Int {
    get { 0 }
    @available(EnabledDomain)
    set { }
  }

  // CHECK: s4Test1SV19dynamicDomainSetterSivg
  // CHECK: s4Test1SV19dynamicDomainSetterSivs
  // CHECK: s4Test1SV19dynamicDomainSetterSivM
  public var dynamicDomainSetter: Int {
    get { 0 }
    @available(DynamicDomain)
    set { }
  }
}

public struct ModifyAccessors {
  var v = 0

  // CHECK: s4Test15ModifyAccessorsV014disabledDomainB0Sivg
  // CHECK-NOT: s4Test15ModifyAccessorsV014disabledDomainB0Siv{{s|M}}
  public var disabledDomainModify: Int {
    get { v }
    @available(DisabledDomain)
    _modify { yield &v }
  }

  // CHECK: s4Test15ModifyAccessorsV013enabledDomainB0Sivg
  // CHECK: s4Test15ModifyAccessorsV013enabledDomainB0SivM
  // CHECK: s4Test15ModifyAccessorsV013enabledDomainB0Sivs
  public var enabledDomainModify: Int {
    get { v }
    @available(EnabledDomain)
    _modify { yield &v }
  }
}

@propertyWrapper
public struct Wrapper {
  var v: Int
  public init(wrappedValue: Int) { v = wrappedValue }

  // CHECK: s4Test7WrapperV12wrappedValueSivg
  // CHECK-NOT: s4Test7WrapperV12wrappedValueSiv{{s|M}}
  // CHECK-NOT: s4Test7WrapperV14projectedValueSivg
  public var wrappedValue: Int {
    get { v }
    @available(DisabledDomain)
    set { v = newValue }
  }

  @available(DisabledDomain)
  public var projectedValue: Int { v }
}

public struct WrappedProperties {
  // CHECK: s4Test17WrappedPropertiesV7wrappedSivg
  // CHECK-NOT: s4Test17WrappedPropertiesV7wrappedSiv{{s|M}}
  // CHECK-NOT: s4Test17WrappedPropertiesV8$wrappedSivg
  @Wrapper public var wrapped: Int = 0
}

open class Base {
  public init() { }

  @available(EnabledDomain)
  public init(enabledDomain: Int) { }

  // CHECK-NOT: s4Test4BaseC14disabledDomainACSi_tcf
  @available(DisabledDomain)
  public init(disabledDomain: Int) { }
}

// CHECK: s4Test7DerivedC13enabledDomainACSi_tcfC
// CHECK-NOT: s4Test7DerivedC14disabledDomainACSi_tcf
open class Derived: Base { }
