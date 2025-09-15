// RUN: %target-swift-frontend -target x86_64-apple-ios13.1-macabi -primary-file %s -emit-ir | %FileCheck %s
// RUN: %target-swift-frontend -target x86_64-apple-ios13.1-macabi -primary-file %s -O -emit-ir | %FileCheck %s --check-prefix=OPT

// REQUIRES: objc_interop
// REQUIRES: OS=macosx || OS=maccatalyst

import Foundation


// We mustn't hoist the alloc_stack for measurement out of the availability
// guard.

// CHECK-LABEL: define{{.*}} @{{.*}}dontHoist
// CHECK-NOT: s10Foundation11MeasurementVySo17NSUnitTemperature
// CHECK: call swiftcc i1 @"$ss33_stdlib_isVariantOSVersionAtLeastyBi1_Bw_BwBwtF"(
// CHECK: s10Foundation11MeasurementVySo17NSUnitTemperature

// OPT-LABEL: define{{.*}} @{{.*}}dontHoist
// OPT-NOT: S10Foundation11MeasurementVySo17NSUnitTemperature
// OPT: call {{.*}} @"$ss33_stdlib_isVariantOSVersionAtLeastyBi1_Bw_BwBwtF"(
// OPT: s10Foundation11MeasurementVySo17NSUnitTemperature

public func dontHoist() {
  if #available(macOS 10.15.4, iOS 13.4, watchOS 6.2, tvOS 13.4, *) {
      let measurement = Measurement<UnitTemperature>(value: Double(42), unit: .celsius)
      print("\(measurement)")
  } else {
      print("Not measurement")
  }
}

// The optimizer is weaker for _isVariantOSVersionAtLeast than for _isOSVersionAtLeast
// because the first is implemented in a runtime call in compiler-rt. We don't want
// to inline the implementation details of the compiler-rt support into apps, so
// _isVariantOSVersionAtLeast not marked as inlinable, but we do mark it as
// _effects(readnone).
// This means that unlike _isOSVersionAtLeast the optimizer can only coalesce
// availability checks with the same availability. It does not determine, for example,
// that a check for iOS 10 is sufficient to guarantee that a check for iOS 9 will also
// succeed.

// With optimizations on, multiple #availability checks should generate only
// a single call into _isVariantOSVersionAtLeast.

// CHECK-LABEL: define{{.*}} @{{.*}}multipleAvailabilityChecks
// CHECK: call swiftcc i1 @"$ss33_stdlib_isVariantOSVersionAtLeastyBi1_Bw_BwBwtF"(
// CHECK: call swiftcc i1 @"$ss33_stdlib_isVariantOSVersionAtLeastyBi1_Bw_BwBwtF"(
// CHECK: call swiftcc i1 @"$ss33_stdlib_isVariantOSVersionAtLeastyBi1_Bw_BwBwtF"(
// CHECK: ret void

// OPT-LABEL: define{{.*}} @{{.*}}multipleAvailabilityChecks
// OPT: call {{.*}} @"$ss33_stdlib_isVariantOSVersionAtLeastyBi1_Bw_BwBwtF"
// OPT-NOT: call {{.*}} @"$ss33_stdlib_isVariantOSVersionAtLeastyBi1_Bw_BwBwtF"
// OPT: ret void
public func multipleAvailabilityChecks() {
  if #available(macOS 10.15.4, iOS 13.4, watchOS 6.2, tvOS 13.4, *) {
    print("test one")
  }
  if #available(macOS 10.15.4, iOS 13.4, watchOS 6.2, tvOS 13.4, *) {
    print("test two")
  }
  if #available(macOS 10.15.4, iOS 13.4, watchOS 6.2, tvOS 13.4, *) {
    print("test three")
  }
}
