// RUN: %target-swift-frontend -enable-experimental-feature TargetAttribute -emit-ir -parse-as-library -target x86_64-apple-macosx13.0 %s | %FileCheck %s

// REQUIRES: swift_feature_TargetAttribute
// REQUIRES: OS=macosx && CPU=x86_64

// CHECK: define {{.*}}@"$s{{.*}}withFeature{{.*}}"(i64 %0) [[FEATURE_ATTRS:#[0-9]+]]
@_target("avx2")
public func withFeature(_ x: Int) -> Int {
    return x + 1
}

// CHECK: define {{.*}}@"$s{{.*}}withArch{{.*}}"(i64 %0) [[ARCH_ATTRS:#[0-9]+]]
@_target("arch=skylake")
public func withArch(_ x: Int) -> Int {
    return x + 1
}

// CHECK-DAG: attributes [[FEATURE_ATTRS]] = {{.*}}"target-features"="{{[^"]*}}+avx2{{[^"]*}}"

// CHECK-DAG: attributes [[ARCH_ATTRS]] = {{.*}}"target-cpu"="skylake"{{.*}}"target-features"="{{[^"]*}}"
