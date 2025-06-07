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

// CHECK-NOT: call swiftcc void @never()

// CHECK: call swiftcc void @always()
// CHECK-NOT: call swiftcc void @never()
if #available(EnabledDomain) {
  always()
} else {
  never()
}

// CHECK: call swiftcc void @always()
// CHECK-NOT: call swiftcc void @never()
if #available(DisabledDomain) {
  never()
} else {
  always()
}

// CHECK: call swiftcc void @always()
// CHECK-NOT: call swiftcc void @never()
if #unavailable(EnabledDomain) {
  never()
} else {
  always()
}

// CHECK: call swiftcc void @always()
// CHECK-NOT: call swiftcc void @never()
if #unavailable(DisabledDomain) {
  always()
} else {
  never()
}
