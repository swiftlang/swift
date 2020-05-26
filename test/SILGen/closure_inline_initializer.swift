// RUN: %target-swift-emit-silgen %s | %FileCheck %s

// CHECK-LABEL: sil private [ossa] @$s26closure_inline_initializer3FooV3fooSivpfiSiyXEfU_

struct Foo {
  var foo: Int = { 2 }()

  init(x: Int) {}
  init(y: Int) {}
}
