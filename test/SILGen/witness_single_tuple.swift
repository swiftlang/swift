// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-silgen %s | %FileCheck %s

protocol Runcible {
  func runce(x: Int)
}

// CHECK-LABEL: sil hidden [transparent] [thunk] @_T020witness_single_tuple3FooVAA8RuncibleAaaDP5runce{{[_0-9a-zA-Z]*}}FTW
struct Foo: Runcible {
  func runce(x: Int = 0) {}
}
