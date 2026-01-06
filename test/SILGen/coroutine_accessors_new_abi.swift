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
  read {
    yield one
  }
}

}

