// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

func subclassFloatLiteral() -> Bar {
  let x: Bar = 1.0
  return x
}
// CHECK-LABEL: sil hidden @_T013required_init20subclassFloatLiteralAA3BarCyF
// CHECK:         class_method {{%.*}} : $@thick Foo.Type, #Foo.init!allocator.1

class Foo: ExpressibleByFloatLiteral {
  required init(floatLiteral: Float) { }

  func identify() {}
}

class Bar: Foo {
  override func identify() {}
}

// CHECK-LABEL: sil_vtable Foo {
// CHECK:         #Foo.init!allocator.1: {{.*}} : _T013required_init3FooC{{[_0-9a-zA-Z]*}}fC
// CHECK:         #Foo.init!initializer.1: {{.*}} : _T013required_init3FooC{{[_0-9a-zA-Z]*}}fc

// CHECK-LABEL: sil_vtable Bar {
// CHECK:         #Foo.init!allocator.1: {{.*}} : _T013required_init3BarC{{[_0-9a-zA-Z]*}}fC
// CHECK:         #Foo.init!initializer.1: {{.*}} : _T013required_init3BarC{{[_0-9a-zA-Z]*}}fc
