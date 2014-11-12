// RUN: %swift -emit-silgen %s | FileCheck %s

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
