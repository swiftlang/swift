// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

protocol Runcible {
  func runce(x: Int)
}

// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV20witness_single_tuple3FooS_8RuncibleS_FS1_5runce
struct Foo: Runcible {
  func runce(x: Int = 0) {}
}
