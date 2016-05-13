// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-build-swift -Xfrontend -enable-reflection-metadata -Xfrontend -enable-reflection-names -lswiftSwiftReflectionTest %s -o %t/existentials
// RUN: %target-run %target-swift-reflection-test %t/existentials | FileCheck %s --check-prefix=CHECK-%target-ptrsize
// REQUIRES: objc_interop

/*
   This file pokes at the swift_reflection_projectExistential API
   of the SwiftRemoteMirror library.

   It tests the three conditions of existential layout:

   - Class existentials
   - Existentials whose contained type fits in the 3-word buffer
   - Existentials whose contained type has to be allocated into a
     raw heap buffer.
   - Error existentials, a.k.a. `ErrorProtocol`.

   - See also: SwiftReflectionTest.reflect(any:)
   - See also: SwiftReflectionTest.reflect(error:)
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

protocol MyProtocol {}
protocol MyErrorProtocol : ErrorProtocol {}

struct MyError : MyProtocol, ErrorProtocol {
  let i = 0xFEDCBA
}
struct MyCustomError : MyProtocol, MyErrorProtocol {}

struct HasError {
  let singleError: ErrorProtocol
  let errorInComposition: protocol<MyProtocol, ErrorProtocol>
  let customError: MyErrorProtocol
  let customErrorInComposition: protocol<MyErrorProtocol, MyProtocol>
}

// This will be projected as a class existential, so its
// size doesn't matter.
var mc = MyClass(x: 1010, y: (2020, 3030))
reflect(any: mc)

// CHECK-64: Reflecting an existential.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-64: Type reference:
// CHECK-64: (bound_generic_class existentials.MyClass
// CHECK-64:   (struct Swift.Int)
// CHECK-64:   (struct Swift.Int))
// CHECK-64: Type info:
// CHECK-64: (reference kind=strong refcounting=native)

// CHECK-32: Reflecting an existential.
// CHECK-32: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-32: Type reference:
// CHECK-32: (bound_generic_class existentials.MyClass
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
// CHECK-64: (bound_generic_struct existentials.MyStruct
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
// CHECK-32: (bound_generic_struct existentials.MyStruct
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
// CHECK-64: (bound_generic_struct existentials.MyStruct
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
// CHECK-32: (bound_generic_struct existentials.MyStruct
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

var he = HasError(singleError: MyError(), errorInComposition: MyError(), customError: MyCustomError(), customErrorInComposition: MyCustomError())
reflect(any: he)

// CHECK-64: Reflecting an existential.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F+]}}
// CHECK-64: Type reference:
// CHECK-64: (struct existentials.HasError)

// CHECK-64: Type info:
// CHECK-64: (struct size=144 alignment=8 stride=144 num_extra_inhabitants=0
// CHECK-64:   (field name=singleError offset=0
// CHECK-64:     (error_existential size=8 alignment=8 stride=8 num_extra_inhabitants=2147483647
// CHECK-64:       (field name=error offset=0
// CHECK-64:         (reference kind=strong refcounting=unknown))))
// CHECK-64:   (field name=errorInComposition offset=8
// CHECK-64:     (opaque_existential size=48 alignment=8 stride=48 num_extra_inhabitants=0
// CHECK-64:       (field name=value offset=0
// CHECK-64:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1))
// CHECK-64:       (field name=value offset=8
// CHECK-64:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1))
// CHECK-64:       (field name=value offset=16
// CHECK-64:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1))
// CHECK-64:       (field name=metadata offset=24
// CHECK-64:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1))
// CHECK-64:       (field name=wtable offset=32
// CHECK-64:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1))
// CHECK-64:       (field name=wtable offset=40
// CHECK-64:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1))))
// CHECK-64:   (field name=customError offset=56
// CHECK-64:     (opaque_existential size=40 alignment=8 stride=40 num_extra_inhabitants=0
// CHECK-64:       (field name=value offset=0
// CHECK-64:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1))
// CHECK-64:       (field name=value offset=8
// CHECK-64:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1))
// CHECK-64:       (field name=value offset=16
// CHECK-64:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1))
// CHECK-64:       (field name=metadata offset=24
// CHECK-64:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1))
// CHECK-64:       (field name=wtable offset=32
// CHECK-64:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1))))
// CHECK-64:   (field name=customErrorInComposition offset=96
// CHECK-64:     (opaque_existential size=48 alignment=8 stride=48 num_extra_inhabitants=0
// CHECK-64:       (field name=value offset=0
// CHECK-64:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1))
// CHECK-64:       (field name=value offset=8
// CHECK-64:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1))
// CHECK-64:       (field name=value offset=16
// CHECK-64:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1))
// CHECK-64:       (field name=metadata offset=24
// CHECK-64:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1))
// CHECK-64:       (field name=wtable offset=32
// CHECK-64:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1))
// CHECK-64:       (field name=wtable offset=40
// CHECK-64:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1)))))

// CHECK-32: Reflecting an existential.
// CHECK-32: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-32: Type reference:
// CHECK-32: (struct existentials.HasError)

// CHECK-32: Type info:
// CHECK-32: (struct size=72 alignment=4 stride=72 num_extra_inhabitants=0
// CHECK-32:   (field name=singleError offset=0
// CHECK-32:     (error_existential size=4 alignment=4 stride=4 num_extra_inhabitants=4096
// CHECK-32:       (field name=error offset=0
// CHECK-32:         (reference kind=strong refcounting=unknown))))
// CHECK-32:   (field name=errorInComposition offset=4
// CHECK-32:     (opaque_existential size=24 alignment=4 stride=24 num_extra_inhabitants=0
// CHECK-32:       (field name=value offset=0
// CHECK-32:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1))
// CHECK-32:       (field name=value offset=4
// CHECK-32:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1))
// CHECK-32:       (field name=value offset=8
// CHECK-32:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1))
// CHECK-32:       (field name=metadata offset=12
// CHECK-32:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1))
// CHECK-32:       (field name=wtable offset=16
// CHECK-32:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1))
// CHECK-32:       (field name=wtable offset=20
// CHECK-32:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1))))
// CHECK-32:   (field name=customError offset=28
// CHECK-32:     (opaque_existential size=20 alignment=4 stride=20 num_extra_inhabitants=0
// CHECK-32:       (field name=value offset=0
// CHECK-32:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1))
// CHECK-32:       (field name=value offset=4
// CHECK-32:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1))
// CHECK-32:       (field name=value offset=8
// CHECK-32:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1))
// CHECK-32:       (field name=metadata offset=12
// CHECK-32:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1))
// CHECK-32:       (field name=wtable offset=16
// CHECK-32:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1))))
// CHECK-32:   (field name=customErrorInComposition offset=48
// CHECK-32:     (opaque_existential size=24 alignment=4 stride=24 num_extra_inhabitants=0
// CHECK-32:       (field name=value offset=0
// CHECK-32:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1))
// CHECK-32:       (field name=value offset=4
// CHECK-32:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1))
// CHECK-32:       (field name=value offset=8
// CHECK-32:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1))
// CHECK-32:       (field name=metadata offset=12
// CHECK-32:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1))
// CHECK-32:       (field name=wtable offset=16
// CHECK-32:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1))
// CHECK-32:       (field name=wtable offset=20
// CHECK-32:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1)))))

reflect(error: MyError())

// CHECK-64: Reflecting an error existential.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-64: Type reference:
// CHECK-64: (struct existentials.MyError)

// CHECK-64: Type info:
// CHECK-64: (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0
// CHECK-64:   (field name=i offset=0
// CHECK-64:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0
// CHECK-64:       (field offset=0
// CHECK-64:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0)))))

// CHECK-32: Reflecting an error existential.
// CHECK-32: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-32: Type reference:
// CHECK-32: (struct existentials.MyError)

// CHECK-32: Type info:
// CHECK-32: (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0
// CHECK-32:   (field name=i offset=0
// CHECK-32:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0
// CHECK-32:       (field offset=0
// CHECK-32:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0)))))

doneReflecting()
