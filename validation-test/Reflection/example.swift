// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-build-swift -Xfrontend -enable-reflection-metadata -Xfrontend -enable-reflection-names -lswiftSwiftReflectionTest %s -o %T/sketch
// RUN: %target-swift-reflection-test %T/sketch 2>&1 | FileCheck %s
// REQUIRES: objc_interop

import SwiftReflectionTest

class Container {
  var x: Int
  var y: Int
  var z: Container?
  init(x: Int, y: Int, z: Container? = nil) {
    self.x = x
    self.y = y
    self.z = z
  }
}

class MyClass<T, U> {
  let f: () -> ()
  var x: T
  var y: U
  init(x: T, y: U, f: () -> ()) {
    self.x = x
    self.y = y
    self.f = f
  }
  func doFoo() {
    f()
  }
}

var lhs = Container(x: 111, y: 222, z: nil)
var rhs = Container(x: 111, y: 222, z: nil)

func doFoo() {
  lhs.x *= 10
  rhs.x *= 10
  print(lhs)
  print(rhs)
}

var mc = MyClass(x: lhs, y: rhs, f: doFoo)

reflect(object: mc)
reflect(any: mc)
doneReflecting()

// CHECK: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK: isa pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK: Type reference from metadata:
// CHECK: (bound_generic_class sketch.MyClass
// CHECK:   (class sketch.Container)
// CHECK:   (class sketch.Container))
// CHECK: Type reference:
// CHECK: (bound_generic_class sketch.MyClass
// CHECK:   (class sketch.Container)
// CHECK:   (class sketch.Container))
// CHECK: Type info:
// CHECK: (class_instance size=48 alignment=16 stride=48 num_extra_inhabitants=0
// CHECK:   (field name=f offset=16
// CHECK:     (thick_function size=16 alignment=8 stride=16 num_extra_inhabitants=0
// CHECK:       (field name=function offset=0
// CHECK:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1))
// CHECK:       (field name=context offset=8
// CHECK:         (reference kind=strong refcounting=native))))
// CHECK:   (field name=x offset=32
// CHECK:     (reference kind=strong refcounting=native))
// CHECK:   (field name=y offset=40
// CHECK:     (reference kind=strong refcounting=native)))
// CHECK: Reflecting an existential.
// CHECK: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK: isa pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK: Type reference from metadata:
// CHECK: <null type reference>

