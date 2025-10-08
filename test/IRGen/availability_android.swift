// RUN: %target-swift-frontend -target aarch64-unknown-linux-android24 -primary-file %s -emit-ir | %FileCheck %s
// RUN: %target-swift-frontend -target aarch64-unknown-linux-android24 -primary-file %s -O -emit-ir | %FileCheck %s --check-prefix=OPT

// REQUIRES: OS=linux-android || OS=linux-androideabi

// CHECK-LABEL: define{{.*}}$s20availability_android0A5CheckyyF
// CHECK: call swiftcc i1 @"$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF"(

// OPT-LABEL: define{{.*}}$s20availability_android0A5CheckyyF
// OPT: call {{.*}} @"$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF"(

public func availabilityCheck() {
  if #available(Android 28, *) {
    print("test")
  }
}
