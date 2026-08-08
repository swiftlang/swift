// REQUIRES: swift_feature_CoroutineAccessors
// RUN: %target-swift-frontend %s -g -c -O -o - -emit-irgen -enable-experimental-feature CoroutineAccessors -enable-callee-allocated-coro-abi | %FileCheck %s

// This test checks to made sure that the yielding borrow accessor s26CoroutineAccessorsDebugLoc1SV3irmSivy (spelled `_read` in source, but using the yield_once_2 ABI since the CoroutineAccessors feature is enabled) that has a call to @llvm.coro.id.retcon.once.dynamic, also has a debug location set.
// The yield_once_2 ABI is opted into explicitly since it is not the default on every platform.

// CHECK-LABEL: @"$s26CoroutineAccessorsDebugLoc1SV3irmSivy"
// CHECK: @llvm.coro.id.retcon.once.dynamic({{.*}}), !dbg ![[DBGLOC:[0-9]+]]
// CHECK-NEXT: @llvm.coro.begin({{.*}}), !dbg ![[DBGLOC]]

public struct S {
public var o: any AnyObject
public var _i: Int = 0

public var irm: Int {
  _read {
    yield _i
  }
} // public var irm
}
