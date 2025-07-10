// RUN: %target-swift-emit-irgen -module-name Test %s -verify \
// RUN:   -enable-experimental-feature CustomAvailability \
// RUN:   -define-enabled-availability-domain EnabledDomain \
// RUN:   -define-disabled-availability-domain DisabledDomain \
// RUN:   -Onone | %FileCheck %s --check-prefixes=CHECK

// RUN: %target-swift-emit-irgen -module-name Test %s -verify \
// RUN:   -enable-experimental-feature CustomAvailability \
// RUN:   -define-enabled-availability-domain EnabledDomain \
// RUN:   -define-disabled-availability-domain DisabledDomain \
// RUN:   -O | %FileCheck %s --check-prefixes=CHECK

// REQUIRES: swift_feature_CustomAvailability

@_silgen_name("always")
public func always()

@_silgen_name("never")
public func never()

// CHECK-LABEL: define swiftcc void @"$s4Test24ifAvailableEnabledDomainyyF"()
// CHECK: call swiftcc void @always()
// CHECK-NOT: call swiftcc void @never()
public func ifAvailableEnabledDomain() {
  if #available(EnabledDomain) {
    always()
  } else {
    never()
  }
}

// CHECK-LABEL: define swiftcc void @"$s4Test25ifAvailableDisabledDomainyyF"()
// CHECK-NOT: call swiftcc void @never()
// CHECK: call swiftcc void @always()
public func ifAvailableDisabledDomain() {
  if #available(DisabledDomain) {
    never()
  } else {
    always()
  }
}

// CHECK-LABEL: define swiftcc void @"$s4Test26ifUnavailableEnabledDomainyyF"()
// CHECK-NOT: call swiftcc void @never()
// CHECK: call swiftcc void @always()
public func ifUnavailableEnabledDomain() {
  if #unavailable(EnabledDomain) {
    never()
  } else {
    always()
  }
}

// CHECK-LABEL: define swiftcc void @"$s4Test27ifUnavailableDisabledDomainyyF"()
// CHECK: call swiftcc void @always()
// CHECK-NOT: call swiftcc void @never()
public func ifUnavailableDisabledDomain() {
  if #unavailable(DisabledDomain) {
    always()
  } else {
    never()
  }
}
