// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

// CHECK-LABEL: sil shared @_TFIvV26closure_inline_initializer3Foo3fooSiiU_FT_Si

struct Foo {
  var foo: Int = { 2 }()

  init(x: Int) {}
  init(y: Int) {}
}
