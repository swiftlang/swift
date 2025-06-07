// RUN: %target-swift-frontend -primary-file %s -emit-ir | %FileCheck %s
// RUN: %target-swift-frontend -primary-file %s -O -emit-ir | %FileCheck %s --check-prefix=OPT

// On iOS _stdlib_isOSVersionAtLeast() is @_transparent, which affects optimization.
// See IRGen/availability.swift for other Apple platforms.
// REQUIRES: OS=ios

import Foundation

// We mustn't hoist the alloc_stack for measurement out of the availability
// guard.

// CHECK-LABEL: define{{.*}} @{{.*}}dontHoist
// CHECK-NOT: s10Foundation11MeasurementVySo17NSUnitTemperature
// CHECK: call swiftcc i1 @"$ss31_stdlib_isOSVersionAtLeast_AEICyBi1_Bw_BwBwtF"(
// CHECK: s10Foundation11MeasurementVySo17NSUnitTemperature

// OPT-LABEL: define{{.*}} @{{.*}}dontHoist
// OPT-NOT: S10Foundation11MeasurementVySo17NSUnitTemperature
// OPT: call {{.*}} @__isPlatformVersionAtLeast(
// OPT: s10Foundation11MeasurementVySo17NSUnitTemperature

public func dontHoist() {
  if #available(macOS 51.0, iOS 54.0, watchOS 57.0, tvOS 54.0, visionOS 51.1, *) {
      let measurement = Measurement<UnitTemperature>(value: Double(42), unit: .celsius)
      print("\(measurement)")
  } else {
      print("Not measurement")
  }
}


// Now that _isOSVersionAtLeast is no longer inlinable, we do still
// mark it as _effects(readnone).
// This means that unlike in the past the optimizer can now only coalesce
// availability checks with the same availability. It does not determine,
// for example, that a check for iOS 10 is sufficient to guarantee that a check
// for iOS 9 will also succeed.

// With optimizations on, multiple #availability checks should generate only
// a single call into _isOSVersionAtLeast, which after inlining will be a
// call to __isPlatformVersionAtLeast.

// CHECK-LABEL: define{{.*}} @{{.*}}multipleAvailabilityChecks
// CHECK: call swiftcc i1 @"$ss31_stdlib_isOSVersionAtLeast_AEICyBi1_Bw_BwBwtF"(
// CHECK: call swiftcc i1 @"$ss31_stdlib_isOSVersionAtLeast_AEICyBi1_Bw_BwBwtF"(
// CHECK: call swiftcc i1 @"$ss31_stdlib_isOSVersionAtLeast_AEICyBi1_Bw_BwBwtF"(
// CHECK: ret void

// OPT-LABEL: define{{.*}} @{{.*}}multipleAvailabilityChecks
// OPT: call {{.*}} @__isPlatformVersionAtLeast
// OPT-NOT: call {{.*}} @$__isPlatformVersionAtLeast
// OPT: ret void
public func multipleAvailabilityChecks() {
  if #available(macOS 51.0, iOS 54.0, watchOS 57.0, tvOS 54.0, visionOS 51.1, *) {
    print("test one")
  }
  if #available(macOS 51.0, iOS 54.0, watchOS 57.0, tvOS 54.0, visionOS 51.1, *) {
    print("test two")
  }
  if #available(macOS 51.0, iOS 54.0, watchOS 57.0, tvOS 54.0, visionOS 51.1, *) {
    print("test three")
  }
}
