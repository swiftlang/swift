// RUN: %target-swift-emit-silgen %s | %FileCheck %s

protocol Runcible {
  func runce(x: Int)
}

// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s20witness_single_tuple3FooVAA8RuncibleA2aDP5runce{{[_0-9a-zA-Z]*}}FTW
struct Foo: Runcible {
  func runce(x: Int = 0) {}
}
