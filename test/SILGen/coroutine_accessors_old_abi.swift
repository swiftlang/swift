// RUN: %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values -Xllvm -sil-print-types %s -disable-callee-allocated-coro-abi -enable-library-evolution -enable-experimental-feature CoroutineAccessors
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types   \
// RUN:     %s                                              \
// RUN:     -disable-callee-allocated-coro-abi              \
// RUN:     -enable-library-evolution                       \
// RUN:     -enable-experimental-feature CoroutineAccessors \
// RUN: | %FileCheck %s --check-prefixes=CHECK,CHECK-NORMAL

// REQUIRES: swift_feature_CoroutineAccessors

// CHECK: yield_once
// CHECK-NOT: yield_once_2

struct S {

var zero: Int = 0
var i: Int {
  yielding borrow {
    yield zero
  }
}

}
