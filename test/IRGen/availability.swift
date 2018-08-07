// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -primary-file %s -emit-ir | %FileCheck %s
// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -primary-file %s -O -emit-ir | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

// We mustn't hoist the alloc_stack for measurement out of the availability
// guard.

// CHECK-LABEL: define{{.*}} @{{.*}}dontHoist
// CHECK-NOT: S10Foundation11MeasurementVySo17NSUnitTemperature
// CHECK: call swiftcc i1 @"$Ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF"(
// CHECK: S10Foundation11MeasurementVySo17NSUnitTemperature

public func dontHoist() {
  if #available(OSX 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *) {
      let measurement = Measurement<UnitTemperature>(value: Double(42), unit: .celsius)
      print("\(measurement)")
  } else {
      print("Not measurement")
  }
}
