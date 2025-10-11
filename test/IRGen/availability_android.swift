// RUN: %target-swift-frontend -target aarch64-unknown-linux-android24 -primary-file %s -emit-ir -parse-stdlib | %FileCheck %s

// CHECK-LABEL: define{{.*}}$s20availability_android0A5CheckSSyF
// CHECK: call swiftcc i1 @"$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF"(

import Swift

public func availabilityCheck() -> String {
  if #available(Android 28, *) {
    return "28"
  } else {
    return "not 28"
  }
}
