// RUN: %target-swift-emit-sil -module-name Test %s -verify -parse-as-library \
// RUN:   -enable-experimental-feature CustomAvailability \
// RUN:   -define-enabled-availability-domain EnabledDomain \
// RUN:   -define-always-enabled-availability-domain AlwaysEnabledDomain \
// RUN:   -define-disabled-availability-domain DisabledDomain \
// RUN:   -define-dynamic-availability-domain DynamicDomain \
// RUN:   -Onone \
// RUN:   | %FileCheck %s --check-prefixes=CHECK,CHECK-NOOPT

// REQUIRES: swift_feature_CustomAvailability

public enum Enum {
  case alwaysAvailable

  @available(*, unavailable)
  case alwaysUnavailable

  @available(EnabledDomain)
  case enabled

  @available(EnabledDomain, unavailable)
  case enabledUnavailable

  @available(AlwaysEnabledDomain)
  case alwaysEnabled

  @available(AlwaysEnabledDomain, unavailable)
  case alwaysEnabledUnavailable

  @available(DisabledDomain)
  case disabled

  @available(DisabledDomain, unavailable)
  case disabledUnavailable

  @available(DynamicDomain)
  case dynamic

  @available(DynamicDomain, unavailable)
  case dynamicUnavailable
}

// CHECK-LABEL: sil @$s4Test22testFullyCoveredSwitchyyAA4EnumOF : $@convention(thin) (Enum) -> () {
// CHECK:         switch_enum %0, case #Enum.alwaysAvailable!enumelt: {{bb[0-9]+}}, case #Enum.alwaysUnavailable!enumelt: {{bb[0-9]+}}, case #Enum.enabled!enumelt: {{bb[0-9]+}}, case #Enum.alwaysEnabled!enumelt: {{bb[0-9]+}}, case #Enum.disabledUnavailable!enumelt: {{bb[0-9]+}}, case #Enum.dynamic!enumelt: {{bb[0-9]+}}, case #Enum.dynamicUnavailable!enumelt: {{bb[0-9]+}}, default [[DEFAULTBB:bb[0-9]+]]
// CHECK:       [[DEFAULTBB]]:
// CHECK-NEXT:    integer_literal $Builtin.Int1, -1
// CHECK-NEXT:    cond_fail {{%.*}}, "unexpected enum value"
// CHECK-NEXT:    unreachable
// CHECK:       } // end sil function '$s4Test22testFullyCoveredSwitchyyAA4EnumOF'
public func testFullyCoveredSwitch(_ e: Enum) {
  switch e {
  case .alwaysAvailable: ()
  case .alwaysUnavailable: ()
  case .enabled: ()
  case .enabledUnavailable: ()
  case .alwaysEnabled: ()
  case .alwaysEnabledUnavailable: ()
  case .disabled: ()
  case .disabledUnavailable: ()
  case .dynamic: ()
  case .dynamicUnavailable: ()
  }
}
