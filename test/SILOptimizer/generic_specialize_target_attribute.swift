// RUN: %target-swift-frontend -O -emit-ir -parse-as-library -enable-experimental-feature TargetAttribute -target arm64-apple-macosx13.0 %s | %FileCheck %s

// REQUIRES: swift_feature_TargetAttribute
// REQUIRES: OS=macosx && CPU=arm64

@_target("sve2")
public func specializedTargetFn<T>(_ x: T) -> T {
    return x
}

public func forcesSpecialization(_ x: Int) -> Int {
    return specializedTargetFn(x)
}

// CHECK-DAG: define {{.*}}@"$s{{.*}}specializedTargetFn{{.*}}"{{.*}} [[SPECIALIZED_ATTRS:#[0-9]+]]
// CHECK-DAG: attributes [[SPECIALIZED_ATTRS]] = {{.*}}"target-features"="{{[^"]*}}+sve2{{[^"]*}}"
