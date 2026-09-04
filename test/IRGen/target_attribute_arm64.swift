// RUN: %target-swift-frontend -enable-experimental-feature TargetAttribute -emit-ir -parse-as-library -target arm64-apple-macosx13.0 %s | %FileCheck %s

// REQUIRES: swift_feature_TargetAttribute
// REQUIRES: OS=macosx && CPU=arm64

// CHECK: define {{.*}}@"$s{{.*}}withFeature{{.*}}"(i64 %0) [[FEATURE_ATTRS:#[0-9]+]]
@_target("sve2")
public func withFeature(_ x: Int) -> Int {
    return x + 1
}

// CHECK: define {{.*}}@"$s{{.*}}withCPU{{.*}}"(i64 %0) [[CPU_ATTRS:#[0-9]+]]
@_target("cpu=apple-a10")
public func withCPU(_ x: Int) -> Int {
    return x + 1
}

// CHECK-DAG: attributes [[FEATURE_ATTRS]] = {{.*}}"target-features"="{{[^"]*}}+sve2{{[^"]*}}"

// CHECK-DAG: attributes [[CPU_ATTRS]] = {{.*}}"target-cpu"="apple-a10"{{.*}}"target-features"="{{[^"]*}}"
