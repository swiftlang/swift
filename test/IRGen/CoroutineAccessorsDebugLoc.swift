// REQUIRES: swift_feature_CoroutineAccessors
// RUN: %target-swift-frontend %s -g -c -O -o - -emit-irgen -enable-experimental-feature CoroutineAccessors | %FileCheck %s

// This test checks to made sure that the ReadAccessor s26CoroutineAccessorsDebugLoc1SV3irmSivr that has a call to @llvm.coro.id.retcon.once, also has a debug location set.

// CHECK-LABEL: @"$s26CoroutineAccessorsDebugLoc1SV3irmSivr"
// CHECK: %{{.*}} = call token ({{.*}}) @llvm.coro.id.retcon.once({{.*}}), !dbg ![[DBGLOC:[0-9]+]]
// CHECK-NEXT: %{{.*}} = call ptr @llvm.coro.begin({{.*}}), !dbg ![[DBGLOC]]

public struct S {
public var o: any AnyObject
public var _i: Int = 0

public var irm: Int {
  _read {
    yield _i
  }
} // public var irm
}
