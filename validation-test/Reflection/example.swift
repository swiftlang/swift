// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-build-swift -Xfrontend -enable-reflection-metadata -Xfrontend -enable-reflection-names -lswiftSwiftReflectionTest %s -o %t/example
// RUN: %target-run %target-swift-reflection-test %t/example | FileCheck %s --check-prefix=CHECK-%target-ptrsize
// REQUIRES: objc_interop

// rdar://problem/26230879
// REQUIRES: OS=macosx

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

// CHECK-64: Reflecting an object.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}

// CHECK-64: Type reference:
// CHECK-64: (bound_generic_class example.MyClass
// CHECK-64:   (class example.Container)
// CHECK-64:   (class example.Container))

// CHECK-64: Type info:
// CHECK-64: (class_instance size=48 alignment=16 stride=48 num_extra_inhabitants=0
// CHECK-64:   (field name=f offset=16
// CHECK-64:     (thick_function size=16 alignment=8 stride=16 num_extra_inhabitants=0
// CHECK-64:       (field name=function offset=0
// CHECK-64:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1))
// CHECK-64:       (field name=context offset=8
// CHECK-64:         (reference kind=strong refcounting=native))))
// CHECK-64:   (field name=x offset=32
// CHECK-64:     (reference kind=strong refcounting=native))
// CHECK-64:   (field name=y offset=40
// CHECK-64:     (reference kind=strong refcounting=native)))


// CHECK-32: Reflecting an object.
// CHECK-32: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}

// CHECK-32: Type reference:
// CHECK-32: (bound_generic_class example.MyClass
// CHECK-32:   (class example.Container)
// CHECK-32:   (class example.Container))

// CHECK-32: Type info:
// CHECK-32: (class_instance size=28 alignment=16 stride=32 num_extra_inhabitants=0
// CHECK-32:   (field name=f offset=12
// CHECK-32:     (thick_function size=8 alignment=4 stride=8 num_extra_inhabitants=0
// CHECK-32:       (field name=function offset=0
// CHECK-32:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1))
// CHECK-32:       (field name=context offset=4
// CHECK-32:         (reference kind=strong refcounting=native))))
// CHECK-32:   (field name=x offset=20
// CHECK-32:     (reference kind=strong refcounting=native))
// CHECK-32:   (field name=y offset=24
// CHECK-32:     (reference kind=strong refcounting=native)))

reflect(any: mc)

// CHECK-64: Reflecting an existential.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-64: Type reference:
// CHECK-64: (bound_generic_class example.MyClass
// CHECK-64:   (class example.Container)
// CHECK-64:   (class example.Container))

// CHECK-64: Type info:
// CHECK-64: (reference kind=strong refcounting=native)


// CHECK-32: Reflecting an existential.
// CHECK-32: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-32: Type reference:
// CHECK-32: (bound_generic_class example.MyClass
// CHECK-32:   (class example.Container)
// CHECK-32:   (class example.Container))

// CHECK-32: Type info:
// CHECK-32: (reference kind=strong refcounting=native)

doneReflecting()

