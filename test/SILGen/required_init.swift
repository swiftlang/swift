// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

func subclassFloatLiteral() -> Bar {
  let x: Bar = 1.0
  return x
}
// CHECK-LABEL: sil hidden @_TF13required_init20subclassFloatLiteralFT_CS_3Bar
// CHECK:         class_method {{%.*}} : $@thick Foo.Type, #Foo.init!allocator.1

class Foo: ExpressibleByFloatLiteral {
  required init(floatLiteral: Float) { }

  func identify() {}
}

class Bar: Foo {
  override func identify() {}
}

// CHECK-LABEL: sil_vtable Foo {
// CHECK:         #Foo.init!allocator.1: _TFC13required_init3FooC
// CHECK:         #Foo.init!initializer.1: _TFC13required_init3Fooc

// CHECK-LABEL: sil_vtable Bar {
// CHECK:         #Foo.init!allocator.1: _TFC13required_init3BarC
// CHECK:         #Foo.init!initializer.1: _TFC13required_init3Barc
