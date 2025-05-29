// RUN: %target-swift-emit-irgen -module-name Test %s -verify \
// RUN:   -enable-experimental-feature CustomAvailability \
// RUN:   -define-enabled-availability-domain EnabledDomain \
// RUN:   -define-disabled-availability-domain DisabledDomain \
// RUN:   -target %target-cpu-apple-macosx13 \
// RUN:   -target-variant %target-cpu-apple-ios16-macabi \
// RUN:   -Onone | %FileCheck %s --check-prefixes=CHECK

// RUN: %target-swift-emit-irgen -module-name Test %s -verify \
// RUN:   -enable-experimental-feature CustomAvailability \
// RUN:   -define-enabled-availability-domain EnabledDomain \
// RUN:   -define-disabled-availability-domain DisabledDomain \
// RUN:   -target %target-cpu-apple-macosx13 \
// RUN:   -target-variant %target-cpu-apple-ios16-macabi \
// RUN:   -O | %FileCheck %s --check-prefixes=CHECK

// REQUIRES: OS=macosx || OS=maccatalyst
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

// FIXME: [availability] These CHECK lines for if #unavailable are inverted (rdar://147929876)
// CHECK-NOT: call swiftcc void @always()
// CHECK: call swiftcc void @never()
if #unavailable(EnabledDomain) {
  never()
} else {
  always()
}

// CHECK-NOT: call swiftcc void @always()
// CHECK: call swiftcc void @never()
if #unavailable(DisabledDomain) {
  always()
} else {
  never()
}
