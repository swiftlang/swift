// RUN: %target-swift-frontend -emit-ir %s -enable-experimental-feature Embedded | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: VENDOR=apple
// REQUIRES: OS=macosx

struct Foo {
  public subscript(x: Int) -> Int {
    get {
      return 0
    }

    @available(*, unavailable)
    set { }
  }
}

// CHECK: $s4main3FooVyS2icig
// CHECK-NOT: $s4main3FooVyS2icis
