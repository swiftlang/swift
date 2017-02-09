// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-silgen %s | %FileCheck %s

// CHECK-LABEL: sil shared @_T026closure_inline_initializer3FooV3fooSivfiSiycfU_

struct Foo {
  var foo: Int = { 2 }()

  init(x: Int) {}
  init(y: Int) {}
}
