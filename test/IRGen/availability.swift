// RUN: %target-swift-frontend -primary-file %s -emit-ir | %FileCheck %s
// RUN: %target-swift-frontend -primary-file %s -O -emit-ir | %FileCheck %s --check-prefix=OPT

// REQUIRES: objc_interop

import Foundation

// We mustn't hoist the alloc_stack for measurement out of the availability
// guard.

// CHECK-LABEL: define{{.*}} @{{.*}}dontHoist
// CHECK-NOT: s10Foundation11MeasurementVySo17NSUnitTemperature
// CHECK: call swiftcc i1 @"$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF"(
// CHECK: s10Foundation11MeasurementVySo17NSUnitTemperature

// OPT-LABEL: define{{.*}} @{{.*}}dontHoist
// OPT-NOT: S10Foundation11MeasurementVySo17NSUnitTemperature
// OPT: call {{.*}} @"$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF"(
// OPT: s10Foundation11MeasurementVySo17NSUnitTemperature

public func dontHoist() {
  if #available(OSX 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *) {
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
// a single call into _isOSVersionAtLeast.

// CHECK-LABEL: define{{.*}} @{{.*}}multipleAvailabilityChecks
// CHECK: call swiftcc i1 @"$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF"(
// CHECK: call swiftcc i1 @"$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF"(
// CHECK: call swiftcc i1 @"$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF"(
// CHECK: ret void

// OPT-LABEL: define{{.*}} @{{.*}}multipleAvailabilityChecks
// OPT: call {{.*}} @"$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF"
// OPT-NOT: call {{.*}} @"$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF"
// OPT: ret void
public func multipleAvailabilityChecks() {
  if #available(OSX 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *) {
    print("test one")
  }
  if #available(OSX 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *) {
    print("test two")
  }
  if #available(OSX 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *) {
    print("test three")
  }
}
