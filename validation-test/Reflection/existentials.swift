// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-build-swift -Xfrontend -enable-reflection-metadata -Xfrontend -enable-reflection-names -lswiftSwiftReflectionTest %s -o %t/example
// RUN: %target-run %target-swift-reflection-test %t/example 2>&1 | FileCheck %s --check-prefix=CHECK-%target-ptrsize
// REQUIRES: objc_interop

/*
   This file pokes at the swift_reflection_projectExistential API
   of the SwiftRemoteMirror library.

   It tests the three conditions of existential layout:

   - Class existentials
   - Existentials whose contained type fits in the 3-word buffer
   - Existentials whose contained type has to be allocated into a
     raw heap buffer.

   - See also: SwiftReflectionTest.reflect(any:)
*/

import SwiftReflectionTest

class MyClass<T, U> {
  let x: T
  let y: (T, U)
  init(x: T, y: (T, U)) {
    self.x = x
    self.y = y
  }
}

struct MyStruct<T, U, V> {
  let x: T
  let y: U
  let z: V
}

// This will be projected as a class existential, so its
// size doesn't matter.
var mc = MyClass(x: 1010, y: (2020, 3030))
reflect(any: mc)

// CHECK-64: Reflecting an existential.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-64: Type reference:
// CHECK-64: (bound_generic_class example.MyClass
// CHECK-64:   (struct Swift.Int)
// CHECK-64:   (struct Swift.Int))
// CHECK-64: Type info:
// CHECK-64: (reference kind=strong refcounting=native)

// CHECK-32: Reflecting an existential.
// CHECK-32: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-32: Type reference:
// CHECK-32: (bound_generic_class example.MyClass
// CHECK-32:   (struct Swift.Int)
// CHECK-32:   (struct Swift.Int))
// CHECK-32: Type info:
// CHECK-32: (reference kind=strong refcounting=native)

// This value fits in the 3-word buffer in the container.
var smallStruct = MyStruct(x: 1, y: 2, z: 3)
reflect(any: smallStruct)

// CHECK-64: Reflecting an existential.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-64: Type reference:
// CHECK-64: (bound_generic_struct example.MyStruct
// CHECK-64:   (struct Swift.Int)
// CHECK-64:   (struct Swift.Int)
// CHECK-64:   (struct Swift.Int))
// CHECK-64: Type info:
// CHECK-64: (struct size=24 alignment=8 stride=24 num_extra_inhabitants=0
// CHECK-64:   (field name=x offset=0
// CHECK-64:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0
// CHECK-64:       (field offset=0
// CHECK-64:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0))))
// CHECK-64:   (field name=y offset=8
// CHECK-64:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0
// CHECK-64:       (field offset=0
// CHECK-64:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0))))
// CHECK-64:   (field name=z offset=16
// CHECK-64:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0
// CHECK-64:       (field offset=0
// CHECK-64:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0)))))

// CHECK-32: Reflecting an existential.
// CHECK-32: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-32: Type reference:
// CHECK-32: (bound_generic_struct example.MyStruct
// CHECK-32:   (struct Swift.Int)
// CHECK-32:   (struct Swift.Int)
// CHECK-32:   (struct Swift.Int))
// CHECK-32: Type info:
// CHECK-32: (struct size=12 alignment=4 stride=12 num_extra_inhabitants=0
// CHECK-32:   (field name=x offset=0
// CHECK-32:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0
// CHECK-32:       (field offset=0
// CHECK-32:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0))))
// CHECK-32:   (field name=y offset=4
// CHECK-32:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0
// CHECK-32:       (field offset=0
// CHECK-32:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0))))
// CHECK-32:   (field name=z offset=8
// CHECK-32:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0
// CHECK-32:       (field offset=0
// CHECK-32:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0)))))

// This value will be copied into a heap buffer, with a
// pointer to it in the existential.
var largeStruct = MyStruct(x: (1,1,1), y: (2,2,2), z: (3,3,3))
reflect(any: largeStruct)

// CHECK-64: Reflecting an existential.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-64: Type reference:
// CHECK-64: (bound_generic_struct example.MyStruct
// CHECK-64:   (tuple
// CHECK-64:     (struct Swift.Int)
// CHECK-64:     (struct Swift.Int)
// CHECK-64:     (struct Swift.Int))
// CHECK-64:   (tuple
// CHECK-64:     (struct Swift.Int)
// CHECK-64:     (struct Swift.Int)
// CHECK-64:     (struct Swift.Int))
// CHECK-64:   (tuple
// CHECK-64:     (struct Swift.Int)
// CHECK-64:     (struct Swift.Int)
// CHECK-64:     (struct Swift.Int)))
// CHECK-64: Type info:
// CHECK-64: (struct size=72 alignment=8 stride=72 num_extra_inhabitants=0
// CHECK-64:   (field name=x offset=0
// CHECK-64:     (tuple size=24 alignment=8 stride=24 num_extra_inhabitants=0
// CHECK-64:       (field offset=0
// CHECK-64:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0
// CHECK-64:           (field offset=0
// CHECK-64:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0))))
// CHECK-64:       (field offset=8
// CHECK-64:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0
// CHECK-64:           (field offset=0
// CHECK-64:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0))))
// CHECK-64:       (field offset=16
// CHECK-64:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0
// CHECK-64:           (field offset=0
// CHECK-64:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0))))))
// CHECK-64:   (field name=y offset=24
// CHECK-64:     (tuple size=24 alignment=8 stride=24 num_extra_inhabitants=0
// CHECK-64:       (field offset=0
// CHECK-64:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0
// CHECK-64:           (field offset=0
// CHECK-64:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0))))
// CHECK-64:       (field offset=8
// CHECK-64:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0
// CHECK-64:           (field offset=0
// CHECK-64:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0))))
// CHECK-64:       (field offset=16
// CHECK-64:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0
// CHECK-64:           (field offset=0
// CHECK-64:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0))))))
// CHECK-64:   (field name=z offset=48
// CHECK-64:     (tuple size=24 alignment=8 stride=24 num_extra_inhabitants=0
// CHECK-64:       (field offset=0
// CHECK-64:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0
// CHECK-64:           (field offset=0
// CHECK-64:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0))))
// CHECK-64:       (field offset=8
// CHECK-64:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0
// CHECK-64:           (field offset=0
// CHECK-64:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0))))
// CHECK-64:       (field offset=16
// CHECK-64:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0
// CHECK-64:           (field offset=0
// CHECK-64:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0)))))))

// CHECK-32: Reflecting an existential.
// CHECK-32: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-32: Type reference:
// CHECK-32: (bound_generic_struct example.MyStruct
// CHECK-32:   (tuple
// CHECK-32:     (struct Swift.Int)
// CHECK-32:     (struct Swift.Int)
// CHECK-32:     (struct Swift.Int))
// CHECK-32:   (tuple
// CHECK-32:     (struct Swift.Int)
// CHECK-32:     (struct Swift.Int)
// CHECK-32:     (struct Swift.Int))
// CHECK-32:   (tuple
// CHECK-32:     (struct Swift.Int)
// CHECK-32:     (struct Swift.Int)
// CHECK-32:     (struct Swift.Int)))
// CHECK-32: Type info:
// CHECK-32: (struct size=36 alignment=4 stride=36 num_extra_inhabitants=0
// CHECK-32:   (field name=x offset=0
// CHECK-32:     (tuple size=12 alignment=4 stride=12 num_extra_inhabitants=0
// CHECK-32:       (field offset=0
// CHECK-32:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0
// CHECK-32:           (field offset=0
// CHECK-32:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0))))
// CHECK-32:       (field offset=4
// CHECK-32:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0
// CHECK-32:           (field offset=0
// CHECK-32:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0))))
// CHECK-32:       (field offset=8
// CHECK-32:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0
// CHECK-32:           (field offset=0
// CHECK-32:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0))))))
// CHECK-32:   (field name=y offset=12
// CHECK-32:     (tuple size=12 alignment=4 stride=12 num_extra_inhabitants=0
// CHECK-32:       (field offset=0
// CHECK-32:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0
// CHECK-32:           (field offset=0
// CHECK-32:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0))))
// CHECK-32:       (field offset=4
// CHECK-32:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0
// CHECK-32:           (field offset=0
// CHECK-32:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0))))
// CHECK-32:       (field offset=8
// CHECK-32:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0
// CHECK-32:           (field offset=0
// CHECK-32:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0))))))
// CHECK-32:   (field name=z offset=24
// CHECK-32:     (tuple size=12 alignment=4 stride=12 num_extra_inhabitants=0
// CHECK-32:       (field offset=0
// CHECK-32:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0
// CHECK-32:           (field offset=0
// CHECK-32:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0))))
// CHECK-32:       (field offset=4
// CHECK-32:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0
// CHECK-32:           (field offset=0
// CHECK-32:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0))))
// CHECK-32:       (field offset=8
// CHECK-32:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0
// CHECK-32:           (field offset=0
// CHECK-32:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0)))))))

doneReflecting()
