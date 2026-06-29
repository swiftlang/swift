// RUN: %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values -Xllvm -sil-print-types %s -enable-callee-allocated-coro-abi -enable-library-evolution -enable-experimental-feature CoroutineAccessors
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types   \
// RUN:     %s                                              \
// RUN:     -enable-callee-allocated-coro-abi               \
// RUN:     -enable-library-evolution                       \
// RUN:     -enable-experimental-feature CoroutineAccessors \
// RUN: | %FileCheck %s --check-prefixes=CHECK,CHECK-NORMAL

// REQUIRES: swift_feature_CoroutineAccessors

// CHECK: yield_once_2

struct S {

var one: Int = 1
var i: Int {
  yielding borrow {
    yield one
  }
}

}

