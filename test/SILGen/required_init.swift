// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

func subclassFloatLiteral() -> Bar {
  let x: Bar = 1.0
  return x
}
// CHECK-LABEL: sil hidden @_TF13required_init20subclassFloatLiteralFT_CS_3Bar
// CHECK:         class_method {{%.*}} : $@thick Foo.Type, #Foo.init!allocator.1

class Foo: FloatLiteralConvertible {
  required init(floatLiteral: Float) { }

  func identify() { println("Foo") }
}

class Bar: Foo {
  override func identify() { println("Bar") }
}

// CHECK-LABEL: sil_vtable Foo {
// CHECK:         #Foo.init!allocator.1: _TFC13required_init3FooCfMS0_FT12floatLiteralSf_S0_
// CHECK:         #Foo.init!initializer.1: _TFC13required_init3FoocfMS0_FT12floatLiteralSf_S0_

// CHECK-LABEL: sil_vtable Bar {
// CHECK:         #Foo.init!allocator.1: _TFC13required_init3BarCfMS0_FT12floatLiteralSf_S0_
// CHECK:         #Foo.init!initializer.1: _TFC13required_init3BarcfMS0_FT12floatLiteralSf_S0_
