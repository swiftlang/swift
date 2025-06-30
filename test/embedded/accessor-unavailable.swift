// RUN: %target-swift-frontend -emit-ir %s -enable-experimental-feature Embedded | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

struct Foo {
  public subscript(x: Int) -> Int {
    get {
      return 0
    }

    @available(*, unavailable)
    set { }
  }
}

let foo = Foo()
let _ = foo[5]

// CHECK: $e4main3FooVyS2icig
// CHECK-NOT: $e4main3FooVyS2icis
