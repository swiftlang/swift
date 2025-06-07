// RUN: %target-swift-frontend -primary-file %s -target x86_64-apple-macosx10.50 -target-variant x86_64-apple-ios48.0-macabi -emit-ir | %FileCheck %s
// RUN: %target-swift-frontend -primary-file %s -target x86_64-apple-macosx10.50 -target-variant x86_64-apple-ios48.0-macabi -O -emit-ir | %FileCheck %s --check-prefix=OPT

// REQUIRES: objc_interop
// REQUIRES: OS=macosx || OS=maccatalyst

// With optimizations on, multiple #availability checks should generate only
// a single call into _isVariantOSVersionAtLeast.

// CHECK-LABEL: define{{.*}} @{{.*}}multipleAvailabilityChecks
// CHECK: call swiftcc i1 @"$ss042_stdlib_isOSVersionAtLeastOrVariantVersiondE0yBi1_Bw_BwBwBwBwBwtF"(
// CHECK: call swiftcc i1 @"$ss042_stdlib_isOSVersionAtLeastOrVariantVersiondE0yBi1_Bw_BwBwBwBwBwtF"(
// CHECK: call swiftcc i1 @"$ss042_stdlib_isOSVersionAtLeastOrVariantVersiondE0yBi1_Bw_BwBwBwBwBwtF"(
// CHECK: ret void

// OPT-LABEL: define{{.*}} @{{.*}}multipleAvailabilityChecks
// OPT: call {{.*}} @"$ss042_stdlib_isOSVersionAtLeastOrVariantVersiondE0yBi1_Bw_BwBwBwBwBwtF"
// OPT-NOT: call {{.*}} @"$ss042_stdlib_isOSVersionAtLeastOrVariantVersiondE0yBi1_Bw_BwBwBwBwBwtF"
// OPT: ret void
public func multipleAvailabilityChecks() {
  if #available(OSX 10.52, iOS 50.0, watchOS 43.0, tvOS 50.0, *) {
    print("test one")
  }
  if #available(OSX 10.52, iOS 50.0, watchOS 43.0, tvOS 50.0, *) {
    print("test two")
  }
  if #available(OSX 10.52, iOS 50.0, watchOS 43.0, tvOS 50.0, *) {
    print("test three")
  }
}
