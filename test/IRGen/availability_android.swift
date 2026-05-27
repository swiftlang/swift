// RUN: %target-swift-frontend -target %target-triple24 -primary-file %s -emit-ir | %FileCheck %s --check-prefix=CHECK-24
// RUN: %target-swift-frontend -target %target-triple28 -primary-file %s -emit-ir | %FileCheck %s --check-prefix=CHECK-28

// CHECK-24-LABEL: define{{.*}}$s20availability_android0A5CheckyyF
// CHECK-24: call swiftcc i1 @"$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF"(

// CHECK-28-LABEL: define{{.*}}$s20availability_android0A5CheckyyF
// CHECK-28-NOT: call swiftcc i1 @"$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF"(

// REQUIRES: OS=linux-android

public func availabilityCheck() {
  if #available(Android 28, *) {
    print("test")
  }
}
