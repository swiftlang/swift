// RUN: %target-swift-emit-silgen -enable-sil-ownership %s | %FileCheck %s

protocol Runcible {
  func runce(x: Int)
}

// CHECK-LABEL: sil private [transparent] [thunk] @$s20witness_single_tuple3FooVAA8RuncibleA2aDP5runce{{[_0-9a-zA-Z]*}}FTW
struct Foo: Runcible {
  func runce(x: Int = 0) {}
}
