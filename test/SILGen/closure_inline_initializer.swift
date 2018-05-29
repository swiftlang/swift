// RUN: %target-swift-emit-silgen -enable-sil-ownership %s | %FileCheck %s

// CHECK-LABEL: sil private @$S26closure_inline_initializer3FooV3fooSivpfiSiyXEfU_

struct Foo {
  var foo: Int = { 2 }()

  init(x: Int) {}
  init(y: Int) {}
}
