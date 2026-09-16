// RUN: %target-swift-frontend -O -emit-sil -parse-as-library -enable-experimental-feature TargetAttribute -target arm64-apple-macosx13.0 %s | %FileCheck %s

// REQUIRES: swift_feature_TargetAttribute
// REQUIRES: OS=macosx && CPU=arm64

@_target("sve2")
@inline(__always)
func sve2_callee(_ x: Int) -> Int {
    return x + 1
}

// The callee has @_target but the caller doesn't -- no inlining
// CHECK-LABEL: sil {{.*}}@{{.*}}none_caller{{.*}} : $@convention(thin) (Int) -> Int
// CHECK: function_ref {{.*}}sve2_callee
// CHECK: apply
// CHECK: end sil function
public func none_caller(_ x: Int) -> Int {
    return sve2_callee(x)
}

// Both caller and callee have @_target, but for different features -- no inlining
// CHECK-LABEL: sil {{.*}}@{{.*}}dotprod_caller{{.*}} : $@convention(thin) (Int) -> Int
// CHECK: function_ref {{.*}}sve2_callee
// CHECK: apply
// CHECK: end sil function
@_target("dotprod")
public func dotprod_caller(_ x: Int) -> Int {
    return sve2_callee(x)
}

// Both caller and callee share the same @_target -- inlined
// CHECK-LABEL: sil {{.*}}@{{.*}}sve2_caller{{.*}} : $@convention(thin) (Int) -> Int
// CHECK-NOT: function_ref
// CHECK-NOT: apply
// CHECK: end sil function
@_target("sve2")
public func sve2_caller(_ x: Int) -> Int {
    return sve2_callee(x)
}
