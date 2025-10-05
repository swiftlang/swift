// RUN: %target-swift-frontend -target aarch64-unknown-linux-android24 -primary-file %s -emit-ir | %FileCheck %s
// RUN: %target-swift-frontend -target aarch64-unknown-linux-android24 -primary-file %s -O -emit-ir | %FileCheck %s --check-prefix=OPT

// REQUIRES: OS=linux-android || OS=linux-androideabi

// CHECK-LABEL: define{{.*}} @{{.*}}availabilityCheck
// CHECK: call swiftcc i1 @"$ss31_stdlib_isOSVersionAtLeast_AEICyBi1_Bw_BwBwtF"(

// OPT-LABEL: define{{.*}} @{{.*}}availabilityCheck
// OPT: call {{.*}} @__isOSVersionAtLeast(

public func availabilityCheck() {
  if #available(Android 28, *) {
    print("test")
  }
}
