// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s
class C {}

enum Foo {
  case X(C, Int)
}

// <rdar://problem/16020428>
// CHECK-LABEL: sil hidden @_TF6tuples8matchFooFT1xOS_3Foo_T_
func matchFoo(#x: Foo) {
  switch x {
  case .X(let x):
    ()
  }
}
