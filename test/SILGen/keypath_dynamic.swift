// RUN: %target-swift-emit-silgen -enable-sil-ownership -module-name A %s | %FileCheck %s

struct Foo {
  var x : Int {
    get {
      return 1
    }
    set {
    }
  }

  func foo(_ kp: WritableKeyPath<Foo, Int>) {
  }

  func test() {
    // CHECK: keypath $WritableKeyPath<Foo, Int>, (root $Foo; settable_property $Int,  id @$s1A3FooV1xSivg
    foo(\.x)
  }
}
