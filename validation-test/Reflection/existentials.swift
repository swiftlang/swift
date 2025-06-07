// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/existentials
// RUN: %target-codesign %t/existentials
// RUN: %target-run %target-swift-reflection-test %t/existentials | %FileCheck %s --check-prefix=CHECK-%target-ptrsize %add_num_extra_inhabitants

// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan

/*
   This file pokes at the swift_reflection_projectExistential API
   of the SwiftRemoteMirror library.

   It tests the three conditions of existential layout:

   - Class existentials
   - Existentials whose contained type fits in the 3-word buffer
   - Existentials whose contained type has to be allocated into a
     raw heap buffer.
   - Error existentials, a.k.a. `Error`.

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
protocol MyErrorProtocol : Error {}

struct MyError : MyProtocol, Error {
  let i = 0xFEDCBA
}
struct MyCustomError : MyProtocol, MyErrorProtocol {}

struct HasError {
  let singleError: Error
  let errorInComposition: MyProtocol & Error
  let customError: MyErrorProtocol
  let customErrorInComposition: MyErrorProtocol & MyProtocol
}

// This will be projected as a class existential, so its
// size doesn't matter.
var mc = MyClass(x: 1010, y: (2020, 3030))
reflect(any: mc)

// CHECK-64: Reflecting an existential.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-64:         Type reference:
// CHECK-64:         (bound_generic_class existentials.MyClass
// CHECK-64-NEXT:    (struct Swift.Int)
// CHECK-64-NEXT:    (struct Swift.Int))
// CHECK-64: Type info:
// CHECK-64: (reference kind=strong refcounting=native)
// CHECK-64: Mangled name: $s12existentials7MyClassCyS2iG
// CHECK-64: Demangled name: existentials.MyClass<Swift.Int, Swift.Int>

// CHECK-64: Start of instance data: [[ADDR:0x[0-9a-fA-F]+]]

// CHECK-32: Reflecting an existential.
// CHECK-32: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-32:        Type reference:
// CHECK-32:        (bound_generic_class existentials.MyClass
// CHECK-32-NEXT:   (struct Swift.Int)
// CHECK-32-NEXT:   (struct Swift.Int))
// CHECK-32: Type info:
// CHECK-32: (reference kind=strong refcounting=native)
// CHECK-32: Mangled name: $s12existentials7MyClassCyS2iG
// CHECK-32: Demangled name: existentials.MyClass<Swift.Int, Swift.Int>

// CHECK-32: Start of instance data: [[ADDR:0x[0-9a-fA-F]+]]

// Reflect and unwrap class existential should print the exact same info,
// EXCEPT for the start of instance data address.
reflect(any: mc, shouldUnwrapClassExistential: true)

// CHECK-64: Reflecting an existential and unwrapping class.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-64:         Type reference:
// CHECK-64:         (bound_generic_class existentials.MyClass
// CHECK-64-NEXT:    (struct Swift.Int)
// CHECK-64-NEXT:    (struct Swift.Int))
// CHECK-64: Type info:
// CHECK-64: (reference kind=strong refcounting=native)
// CHECK-64: Mangled name: $s12existentials7MyClassCyS2iG
// CHECK-64: Demangled name: existentials.MyClass<Swift.Int, Swift.Int>

// CHECK-64: Start of instance data: 
// CHECK-64-NOT: ![[ADDR]]

// CHECK-32: Reflecting an existential and unwrapping class.
// CHECK-32: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-32:        Type reference:
// CHECK-32:        (bound_generic_class existentials.MyClass
// CHECK-32-NEXT:   (struct Swift.Int)
// CHECK-32-NEXT:   (struct Swift.Int))
// CHECK-32: Type info:
// CHECK-32: (reference kind=strong refcounting=native)
// CHECK-32: Mangled name: $s12existentials7MyClassCyS2iG
// CHECK-32: Demangled name: existentials.MyClass<Swift.Int, Swift.Int>

// CHECK-32: Start of instance data: 
// CHECK-32-NOT: ![[ADDR]]

// This value fits in the 3-word buffer in the container.
var smallStruct = MyStruct(x: 1, y: 2, z: 3)
reflect(any: smallStruct)

// CHECK-64: Reflecting an existential.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-64:        Type reference:
// CHECK-64:        (bound_generic_struct existentials.MyStruct
// CHECK-64-NEXT:   (struct Swift.Int)
// CHECK-64-NEXT:   (struct Swift.Int)
// CHECK-64-NEXT:   (struct Swift.Int))

// CHECK-64:        Type info:
// CHECK-64:        (struct size=24 alignment=8 stride=24 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:   (field name=x offset=0
// CHECK-64-NEXT:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:       (field name=_value offset=0
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-64-NEXT:   (field name=y offset=8
// CHECK-64-NEXT:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:       (field name=_value offset=0
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-64-NEXT:   (field name=z offset=16
// CHECK-64-NEXT:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:       (field name=_value offset=0
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1)))))
// CHECK-64-NEXT:   Mangled name:  $s12existentials8MyStructVyS3iG
// CHECK-64-NEXT:   Demangled name:  existentials.MyStruct<Swift.Int, Swift.Int, Swift.Int>

// CHECK-64: Start of instance data: [[ADDR:0x[0-9a-fA-F]+]]


// CHECK-32: Reflecting an existential.
// CHECK-32: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-32:        Type reference:
// CHECK-32:        (bound_generic_struct existentials.MyStruct
// CHECK-32-NEXT:   (struct Swift.Int)
// CHECK-32-NEXT:   (struct Swift.Int)
// CHECK-32-NEXT:   (struct Swift.Int))

// CHECK-32:        Type info:
// CHECK-32:        (struct size=12 alignment=4 stride=12 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:   (field name=x offset=0
// CHECK-32-NEXT:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:       (field name=_value offset=0
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-32-NEXT:   (field name=y offset=4
// CHECK-32-NEXT:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:       (field name=_value offset=0
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-32-NEXT:   (field name=z offset=8
// CHECK-32-NEXT:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:       (field name=_value offset=0
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1)))))
// CHECK-32-NEXT:   Mangled name:  $s12existentials8MyStructVyS3iG
// CHECK-32-NEXT:   Demangled name:  existentials.MyStruct<Swift.Int, Swift.Int, Swift.Int>

// CHECK-32: Start of instance data: [[ADDR:0x[0-9a-fA-F]+]]

reflect(any: smallStruct, shouldUnwrapClassExistential: true)

// CHECK-64: Reflecting an existential and unwrapping class.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-64:        Type reference:
// CHECK-64:        (bound_generic_struct existentials.MyStruct
// CHECK-64-NEXT:   (struct Swift.Int)
// CHECK-64-NEXT:   (struct Swift.Int)
// CHECK-64-NEXT:   (struct Swift.Int))

// CHECK-64:        Type info:
// CHECK-64:        (struct size=24 alignment=8 stride=24 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:   (field name=x offset=0
// CHECK-64-NEXT:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:       (field name=_value offset=0
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-64-NEXT:   (field name=y offset=8
// CHECK-64-NEXT:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:       (field name=_value offset=0
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-64-NEXT:   (field name=z offset=16
// CHECK-64-NEXT:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:       (field name=_value offset=0
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1)))))
// CHECK-64-NEXT:   Mangled name:  $s12existentials8MyStructVyS3iG
// CHECK-64-NEXT:   Demangled name:  existentials.MyStruct<Swift.Int, Swift.Int, Swift.Int>

// CHECK-64: Start of instance data: 
// CHECK-64-NOT: ![[ADDR]]

// CHECK-32: Reflecting an existential and unwrapping class.
// CHECK-32: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-32:        Type reference:
// CHECK-32:        (bound_generic_struct existentials.MyStruct
// CHECK-32-NEXT:   (struct Swift.Int)
// CHECK-32-NEXT:   (struct Swift.Int)
// CHECK-32-NEXT:   (struct Swift.Int))

// CHECK-32:        Type info:
// CHECK-32:        (struct size=12 alignment=4 stride=12 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:   (field name=x offset=0
// CHECK-32-NEXT:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:       (field name=_value offset=0
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-32-NEXT:   (field name=y offset=4
// CHECK-32-NEXT:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:       (field name=_value offset=0
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-32-NEXT:   (field name=z offset=8
// CHECK-32-NEXT:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:       (field name=_value offset=0
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1)))))
// CHECK-32-NEXT:   Mangled name:  $s12existentials8MyStructVyS3iG
// CHECK-32-NEXT:   Demangled name:  existentials.MyStruct<Swift.Int, Swift.Int, Swift.Int>

// CHECK-32: Start of instance data: 
// CHECK-32-NOT: ![[ADDR]]

// This value will be copied into a heap buffer, with a
// pointer to it in the existential.
var largeStruct = MyStruct(x: (1,1,1), y: (2,2,2), z: (3,3,3))
reflect(any: largeStruct)

// CHECK-64: Reflecting an existential.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-64:        Type reference:
// CHECK-64:        (bound_generic_struct existentials.MyStruct
// CHECK-64-NEXT:   (tuple
// CHECK-64-NEXT:     (struct Swift.Int)
// CHECK-64-NEXT:     (struct Swift.Int)
// CHECK-64-NEXT:     (struct Swift.Int))
// CHECK-64-NEXT:   (tuple
// CHECK-64-NEXT:     (struct Swift.Int)
// CHECK-64-NEXT:     (struct Swift.Int)
// CHECK-64-NEXT:     (struct Swift.Int))
// CHECK-64-NEXT:   (tuple
// CHECK-64-NEXT:     (struct Swift.Int)
// CHECK-64-NEXT:     (struct Swift.Int)
// CHECK-64-NEXT:     (struct Swift.Int)))
// CHECK-64: Type info:
// CHECK-64-NEXT: (struct size=72 alignment=8 stride=72 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:   (field name=x offset=0
// CHECK-64-NEXT:     (tuple size=24 alignment=8 stride=24 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:       (field offset=0
// CHECK-64-NEXT:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (field name=_value offset=0
// CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-64-NEXT:       (field offset=8
// CHECK-64-NEXT:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (field name=_value offset=0
// CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-64-NEXT:       (field offset=16
// CHECK-64-NEXT:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (field name=_value offset=0
// CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-64-NEXT:   (field name=y offset=24
// CHECK-64-NEXT:     (tuple size=24 alignment=8 stride=24 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:       (field offset=0
// CHECK-64-NEXT:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (field name=_value offset=0
// CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-64-NEXT:       (field offset=8
// CHECK-64-NEXT:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (field name=_value offset=0
// CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-64-NEXT:       (field offset=16
// CHECK-64-NEXT:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (field name=_value offset=0
// CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-64-NEXT:   (field name=z offset=48
// CHECK-64-NEXT:     (tuple size=24 alignment=8 stride=24 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:       (field offset=0
// CHECK-64-NEXT:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (field name=_value offset=0
// CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-64-NEXT:       (field offset=8
// CHECK-64-NEXT:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (field name=_value offset=0
// CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-64-NEXT:       (field offset=16
// CHECK-64-NEXT:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (field name=_value offset=0
// CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1)))))))
// CHECK-64-NEXT:   Mangled name: $s12existentials8MyStructVySi_S2itSi_S2itSi_S2itG
// CHECK-64-NEXT:   Demangled name: existentials.MyStruct<(Swift.Int, Swift.Int, Swift.Int), (Swift.Int, Swift.Int, Swift.Int), (Swift.Int, Swift.Int, Swift.Int)>

// CHECK-64: Start of instance data: [[ADDR:0x[0-9a-fA-F]+]]


// CHECK-32: Reflecting an existential.
// CHECK-32: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-32:        Type reference:
// CHECK-32:        (bound_generic_struct existentials.MyStruct
// CHECK-32-NEXT:   (tuple
// CHECK-32-NEXT:     (struct Swift.Int)
// CHECK-32-NEXT:     (struct Swift.Int)
// CHECK-32-NEXT:     (struct Swift.Int))
// CHECK-32-NEXT:   (tuple
// CHECK-32-NEXT:     (struct Swift.Int)
// CHECK-32-NEXT:     (struct Swift.Int)
// CHECK-32-NEXT:     (struct Swift.Int))
// CHECK-32-NEXT:   (tuple
// CHECK-32-NEXT:     (struct Swift.Int)
// CHECK-32-NEXT:     (struct Swift.Int)
// CHECK-32-NEXT:     (struct Swift.Int)))
// CHECK-32:        Type info:
// CHECK-32:        (struct size=36 alignment=4 stride=36 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:   (field name=x offset=0
// CHECK-32-NEXT:     (tuple size=12 alignment=4 stride=12 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:       (field offset=0
// CHECK-32-NEXT:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:           (field name=_value offset=0
// CHECK-32-NEXT:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-32-NEXT:       (field offset=4
// CHECK-32-NEXT:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:           (field name=_value offset=0
// CHECK-32-NEXT:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-32-NEXT:       (field offset=8
// CHECK-32-NEXT:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:           (field name=_value offset=0
// CHECK-32-NEXT:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-32-NEXT:   (field name=y offset=12
// CHECK-32-NEXT:     (tuple size=12 alignment=4 stride=12 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:       (field offset=0
// CHECK-32-NEXT:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:           (field name=_value offset=0
// CHECK-32-NEXT:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-32-NEXT:       (field offset=4
// CHECK-32-NEXT:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:           (field name=_value offset=0
// CHECK-32-NEXT:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-32-NEXT:       (field offset=8
// CHECK-32-NEXT:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:           (field name=_value offset=0
// CHECK-32-NEXT:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-32-NEXT:   (field name=z offset=24
// CHECK-32-NEXT:     (tuple size=12 alignment=4 stride=12 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:       (field offset=0
// CHECK-32-NEXT:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:           (field name=_value offset=0
// CHECK-32-NEXT:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-32-NEXT:       (field offset=4
// CHECK-32-NEXT:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:           (field name=_value offset=0
// CHECK-32-NEXT:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-32-NEXT:       (field offset=8
// CHECK-32-NEXT:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:           (field name=_value offset=0
// CHECK-32-NEXT:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1)))))))
// CHECK-32-NEXT:   Mangled name: $s12existentials8MyStructVySi_S2itSi_S2itSi_S2itG
// CHECK-32-NEXT:   Demangled name: existentials.MyStruct<(Swift.Int, Swift.Int, Swift.Int), (Swift.Int, Swift.Int, Swift.Int), (Swift.Int, Swift.Int, Swift.Int)>

// CHECK-32: Start of instance data: [[ADDR:0x[0-9a-fA-F]+]]

reflect(any: largeStruct, shouldUnwrapClassExistential: true)

// CHECK-64: Reflecting an existential and unwrapping class.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-64:        Type reference:
// CHECK-64:        (bound_generic_struct existentials.MyStruct
// CHECK-64-NEXT:   (tuple
// CHECK-64-NEXT:     (struct Swift.Int)
// CHECK-64-NEXT:     (struct Swift.Int)
// CHECK-64-NEXT:     (struct Swift.Int))
// CHECK-64-NEXT:   (tuple
// CHECK-64-NEXT:     (struct Swift.Int)
// CHECK-64-NEXT:     (struct Swift.Int)
// CHECK-64-NEXT:     (struct Swift.Int))
// CHECK-64-NEXT:   (tuple
// CHECK-64-NEXT:     (struct Swift.Int)
// CHECK-64-NEXT:     (struct Swift.Int)
// CHECK-64-NEXT:     (struct Swift.Int)))
// CHECK-64: Type info:
// CHECK-64-NEXT: (struct size=72 alignment=8 stride=72 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:   (field name=x offset=0
// CHECK-64-NEXT:     (tuple size=24 alignment=8 stride=24 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:       (field offset=0
// CHECK-64-NEXT:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (field name=_value offset=0
// CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-64-NEXT:       (field offset=8
// CHECK-64-NEXT:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (field name=_value offset=0
// CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-64-NEXT:       (field offset=16
// CHECK-64-NEXT:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (field name=_value offset=0
// CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-64-NEXT:   (field name=y offset=24
// CHECK-64-NEXT:     (tuple size=24 alignment=8 stride=24 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:       (field offset=0
// CHECK-64-NEXT:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (field name=_value offset=0
// CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-64-NEXT:       (field offset=8
// CHECK-64-NEXT:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (field name=_value offset=0
// CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-64-NEXT:       (field offset=16
// CHECK-64-NEXT:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (field name=_value offset=0
// CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-64-NEXT:   (field name=z offset=48
// CHECK-64-NEXT:     (tuple size=24 alignment=8 stride=24 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:       (field offset=0
// CHECK-64-NEXT:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (field name=_value offset=0
// CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-64-NEXT:       (field offset=8
// CHECK-64-NEXT:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (field name=_value offset=0
// CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-64-NEXT:       (field offset=16
// CHECK-64-NEXT:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:           (field name=_value offset=0
// CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1)))))))
// CHECK-64-NEXT:   Mangled name: $s12existentials8MyStructVySi_S2itSi_S2itSi_S2itG
// CHECK-64-NEXT:   Demangled name: existentials.MyStruct<(Swift.Int, Swift.Int, Swift.Int), (Swift.Int, Swift.Int, Swift.Int), (Swift.Int, Swift.Int, Swift.Int)>

// CHECK-64: Start of instance data: 
// CHECK-64-NOT: ![[ADDR]]

// CHECK-32: Reflecting an existential and unwrapping class.
// CHECK-32: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-32:        Type reference:
// CHECK-32:        (bound_generic_struct existentials.MyStruct
// CHECK-32-NEXT:   (tuple
// CHECK-32-NEXT:     (struct Swift.Int)
// CHECK-32-NEXT:     (struct Swift.Int)
// CHECK-32-NEXT:     (struct Swift.Int))
// CHECK-32-NEXT:   (tuple
// CHECK-32-NEXT:     (struct Swift.Int)
// CHECK-32-NEXT:     (struct Swift.Int)
// CHECK-32-NEXT:     (struct Swift.Int))
// CHECK-32-NEXT:   (tuple
// CHECK-32-NEXT:     (struct Swift.Int)
// CHECK-32-NEXT:     (struct Swift.Int)
// CHECK-32-NEXT:     (struct Swift.Int)))
// CHECK-32:        Type info:
// CHECK-32:        (struct size=36 alignment=4 stride=36 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:   (field name=x offset=0
// CHECK-32-NEXT:     (tuple size=12 alignment=4 stride=12 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:       (field offset=0
// CHECK-32-NEXT:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:           (field name=_value offset=0
// CHECK-32-NEXT:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-32-NEXT:       (field offset=4
// CHECK-32-NEXT:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:           (field name=_value offset=0
// CHECK-32-NEXT:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-32-NEXT:       (field offset=8
// CHECK-32-NEXT:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:           (field name=_value offset=0
// CHECK-32-NEXT:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-32-NEXT:   (field name=y offset=12
// CHECK-32-NEXT:     (tuple size=12 alignment=4 stride=12 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:       (field offset=0
// CHECK-32-NEXT:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:           (field name=_value offset=0
// CHECK-32-NEXT:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-32-NEXT:       (field offset=4
// CHECK-32-NEXT:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:           (field name=_value offset=0
// CHECK-32-NEXT:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-32-NEXT:       (field offset=8
// CHECK-32-NEXT:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:           (field name=_value offset=0
// CHECK-32-NEXT:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-32-NEXT:   (field name=z offset=24
// CHECK-32-NEXT:     (tuple size=12 alignment=4 stride=12 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:       (field offset=0
// CHECK-32-NEXT:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:           (field name=_value offset=0
// CHECK-32-NEXT:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-32-NEXT:       (field offset=4
// CHECK-32-NEXT:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:           (field name=_value offset=0
// CHECK-32-NEXT:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-32-NEXT:       (field offset=8
// CHECK-32-NEXT:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:           (field name=_value offset=0
// CHECK-32-NEXT:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1)))))))
// CHECK-32-NEXT:   Mangled name: $s12existentials8MyStructVySi_S2itSi_S2itSi_S2itG
// CHECK-32-NEXT:   Demangled name: existentials.MyStruct<(Swift.Int, Swift.Int, Swift.Int), (Swift.Int, Swift.Int, Swift.Int), (Swift.Int, Swift.Int, Swift.Int)>

// CHECK-32: Start of instance data: 
// CHECK-32-NOT: ![[ADDR]]

// Function type:
reflect(any: {largeStruct})
// CHECK-64: Mangled name: $s12existentials8MyStructVySi_S2itSi_S2itSi_S2itGyc
// CHECK-64: Demangled name: () -> existentials.MyStruct<(Swift.Int, Swift.Int, Swift.Int), (Swift.Int, Swift.Int, Swift.Int), (Swift.Int, Swift.Int, Swift.Int)>
// CHECK-32: Mangled name: $s12existentials8MyStructVySi_S2itSi_S2itSi_S2itGyc
// CHECK-32: Demangled name: () -> existentials.MyStruct<(Swift.Int, Swift.Int, Swift.Int), (Swift.Int, Swift.Int, Swift.Int), (Swift.Int, Swift.Int, Swift.Int)>

// Protocol composition:
protocol P {}
protocol Q {}
protocol Composition : P, Q {}
struct S : Composition {}
func getComposition() -> P & Q { return S() }
reflect(any: getComposition() as P & Q)
// CHECK-64: Mangled name: $s12existentials1P_AA1Qp
// CHECK-64: Demangled name: existentials.P & existentials.Q
// CHECK-32: Mangled name: $s12existentials1P_AA1Qp
// CHECK-32: Demangled name: existentials.P & existentials.Q

// Metatype:
reflect(any: Int.self)
// CHECK-64: Mangled name: $sSiXMt
// CHECK-64: Demangled name: @thin Swift.Int.Type
// CHECK-32: Mangled name: $sSiXMt
// CHECK-32: Demangled name: @thin Swift.Int.Type

protocol WithType {
  associatedtype T
  func f() -> T
}
struct S1 : WithType {
  typealias T = Int
  func f() -> Int { return 0 }
}
func getWithType<T>(_ t: T)  where T: WithType {
  reflect(any: T.self)
}
getWithType(S1())


var he = HasError(singleError: MyError(), errorInComposition: MyError(), customError: MyCustomError(), customErrorInComposition: MyCustomError())
reflect(any: he)

// CHECK-64: Reflecting an existential.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F+]}}
// CHECK-64: Type reference:
// CHECK-64: (struct existentials.HasError)

// CHECK-64:        Type info:
// CHECK-64:        (struct size=144 alignment=8 stride=144
// CHECK-64-NEXT:   (field name=singleError offset=0
// CHECK-64-NEXT:     (error_existential size=8 alignment=8 stride=8 num_extra_inhabitants=[[#num_extra_inhabitants_64bit]] bitwise_takable=1
// CHECK-64-NEXT:       (field name=error offset=0
// CHECK-64-NEXT:         (reference kind=strong refcounting=unknown))))
// CHECK-64-NEXT:   (field name=errorInComposition offset=8
// CHECK-64-NEXT:     (opaque_existential size=48 alignment=8 stride=48 num_extra_inhabitants=[[#num_extra_inhabitants_64bit]] bitwise_takable=1
// CHECK-64-NEXT:       (field name=metadata offset=24
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=[[#num_extra_inhabitants_64bit]] bitwise_takable=1))
// CHECK-64-NEXT:       (field name=wtable offset=32
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1 bitwise_takable=1))
// CHECK-64-NEXT:       (field name=wtable offset=40
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1 bitwise_takable=1))))
// CHECK-64-NEXT:   (field name=customError offset=56
// CHECK-64-NEXT:     (opaque_existential size=40 alignment=8 stride=40 num_extra_inhabitants=[[#num_extra_inhabitants_64bit]] bitwise_takable=1
// CHECK-64-NEXT:       (field name=metadata offset=24
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=[[#num_extra_inhabitants_64bit]] bitwise_takable=1))
// CHECK-64-NEXT:       (field name=wtable offset=32
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1 bitwise_takable=1))))
// CHECK-64-NEXT:   (field name=customErrorInComposition offset=96
// CHECK-64-NEXT:     (opaque_existential size=48 alignment=8 stride=48 num_extra_inhabitants=[[#num_extra_inhabitants_64bit]] bitwise_takable=1
// CHECK-64-NEXT:       (field name=metadata offset=24
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=[[#num_extra_inhabitants_64bit]] bitwise_takable=1))
// CHECK-64-NEXT:       (field name=wtable offset=32
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1 bitwise_takable=1))
// CHECK-64-NEXT:       (field name=wtable offset=40
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1 bitwise_takable=1)))))
// CHECK-64-NEXT:   Mangled name: $s12existentials8HasErrorV
// CHECK-64-NEXT:   Demangled name: existentials.HasError

// CHECK-64: Start of instance data: [[ADDR:0x[0-9a-fA-F]+]]

// CHECK-32: Reflecting an existential.
// CHECK-32: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-32: Type reference:
// CHECK-32: (struct existentials.HasError)

// CHECK-32:        Type info:
// CHECK-32:        (struct size=72 alignment=4 stride=72 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:   (field name=singleError offset=0
// CHECK-32-NEXT:     (error_existential size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:       (field name=error offset=0
// CHECK-32-NEXT:         (reference kind=strong refcounting=unknown))))
// CHECK-32-NEXT:   (field name=errorInComposition offset=4
// CHECK-32-NEXT:     (opaque_existential size=24 alignment=4 stride=24 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:       (field name=metadata offset=12
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1))
// CHECK-32-NEXT:       (field name=wtable offset=16
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1 bitwise_takable=1))
// CHECK-32-NEXT:       (field name=wtable offset=20
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1 bitwise_takable=1))))
// CHECK-32-NEXT:   (field name=customError offset=28
// CHECK-32-NEXT:     (opaque_existential size=20 alignment=4 stride=20 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:       (field name=metadata offset=12
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1))
// CHECK-32-NEXT:       (field name=wtable offset=16
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1 bitwise_takable=1))))
// CHECK-32-NEXT:   (field name=customErrorInComposition offset=48
// CHECK-32-NEXT:     (opaque_existential size=24 alignment=4 stride=24 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:       (field name=metadata offset=12
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1))
// CHECK-32-NEXT:       (field name=wtable offset=16
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1 bitwise_takable=1))
// CHECK-32-NEXT:       (field name=wtable offset=20
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1 bitwise_takable=1)))))
// CHECK-32-NEXT:   Mangled name: $s12existentials8HasErrorV
// CHECK-32-NEXT:   Demangled name: existentials.HasError

// CHECK-32: Start of instance data: [[ADDR:0x[0-9a-fA-F]+]]


reflect(any: he, shouldUnwrapClassExistential: true)

// CHECK-64: Reflecting an existential and unwrapping class.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F+]}}
// CHECK-64: Type reference:
// CHECK-64: (struct existentials.HasError)

// CHECK-64:        Type info:
// CHECK-64:        (struct size=144 alignment=8 stride=144
// CHECK-64-NEXT:   (field name=singleError offset=0
// CHECK-64-NEXT:     (error_existential size=8 alignment=8 stride=8 num_extra_inhabitants=[[#num_extra_inhabitants_64bit]] bitwise_takable=1
// CHECK-64-NEXT:       (field name=error offset=0
// CHECK-64-NEXT:         (reference kind=strong refcounting=unknown))))
// CHECK-64-NEXT:   (field name=errorInComposition offset=8
// CHECK-64-NEXT:     (opaque_existential size=48 alignment=8 stride=48 num_extra_inhabitants=[[#num_extra_inhabitants_64bit]] bitwise_takable=1
// CHECK-64-NEXT:       (field name=metadata offset=24
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=[[#num_extra_inhabitants_64bit]] bitwise_takable=1))
// CHECK-64-NEXT:       (field name=wtable offset=32
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1 bitwise_takable=1))
// CHECK-64-NEXT:       (field name=wtable offset=40
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1 bitwise_takable=1))))
// CHECK-64-NEXT:   (field name=customError offset=56
// CHECK-64-NEXT:     (opaque_existential size=40 alignment=8 stride=40 num_extra_inhabitants=[[#num_extra_inhabitants_64bit]] bitwise_takable=1
// CHECK-64-NEXT:       (field name=metadata offset=24
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=[[#num_extra_inhabitants_64bit]] bitwise_takable=1))
// CHECK-64-NEXT:       (field name=wtable offset=32
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1 bitwise_takable=1))))
// CHECK-64-NEXT:   (field name=customErrorInComposition offset=96
// CHECK-64-NEXT:     (opaque_existential size=48 alignment=8 stride=48 num_extra_inhabitants=[[#num_extra_inhabitants_64bit]] bitwise_takable=1
// CHECK-64-NEXT:       (field name=metadata offset=24
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=[[#num_extra_inhabitants_64bit]] bitwise_takable=1))
// CHECK-64-NEXT:       (field name=wtable offset=32
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1 bitwise_takable=1))
// CHECK-64-NEXT:       (field name=wtable offset=40
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1 bitwise_takable=1)))))
// CHECK-64-NEXT:   Mangled name: $s12existentials8HasErrorV
// CHECK-64-NEXT:   Demangled name: existentials.HasError

// CHECK-64: Start of instance data: 
// CHECK-64-NOT: ![[ADDR]]

// CHECK-32: Reflecting an existential and unwrapping class.
// CHECK-32: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-32: Type reference:
// CHECK-32: (struct existentials.HasError)

// CHECK-32:        Type info:
// CHECK-32:        (struct size=72 alignment=4 stride=72 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:   (field name=singleError offset=0
// CHECK-32-NEXT:     (error_existential size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:       (field name=error offset=0
// CHECK-32-NEXT:         (reference kind=strong refcounting=unknown))))
// CHECK-32-NEXT:   (field name=errorInComposition offset=4
// CHECK-32-NEXT:     (opaque_existential size=24 alignment=4 stride=24 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:       (field name=metadata offset=12
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1))
// CHECK-32-NEXT:       (field name=wtable offset=16
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1 bitwise_takable=1))
// CHECK-32-NEXT:       (field name=wtable offset=20
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1 bitwise_takable=1))))
// CHECK-32-NEXT:   (field name=customError offset=28
// CHECK-32-NEXT:     (opaque_existential size=20 alignment=4 stride=20 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:       (field name=metadata offset=12
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1))
// CHECK-32-NEXT:       (field name=wtable offset=16
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1 bitwise_takable=1))))
// CHECK-32-NEXT:   (field name=customErrorInComposition offset=48
// CHECK-32-NEXT:     (opaque_existential size=24 alignment=4 stride=24 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:       (field name=metadata offset=12
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1))
// CHECK-32-NEXT:       (field name=wtable offset=16
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1 bitwise_takable=1))
// CHECK-32-NEXT:       (field name=wtable offset=20
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1 bitwise_takable=1)))))
// CHECK-32-NEXT:   Mangled name: $s12existentials8HasErrorV
// CHECK-32-NEXT:   Demangled name: existentials.HasError

// CHECK-32: Start of instance data: 
// CHECK-32-NOT: ![[ADDR]]

let error = MyError()
reflect(error: error)

// CHECK-64: Reflecting an error existential.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-64: Type reference:
// CHECK-64: (struct existentials.MyError)

// CHECK-64:        Type info:
// CHECK-64:        (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:   (field name=i offset=0
// CHECK-64-NEXT:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:       (field name=_value offset=0
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1)))))
// CHECK-64-NEXT:   Mangled name: $s12existentials7MyErrorV
// CHECK-64-NEXT:   Demangled name: existentials.MyError

// CHECK-64: Start of instance data: [[ADDR:0x[0-9a-fA-F]+]]

// CHECK-32: Reflecting an error existential.
// CHECK-32: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-32: Type reference:
// CHECK-32: (struct existentials.MyError)

// CHECK-32:        Type info:
// CHECK-32:        (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:   (field name=i offset=0
// CHECK-32-NEXT:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:       (field name=_value offset=0
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1)))))
// CHECK-32-NEXT:   Mangled name: $s12existentials7MyErrorV
// CHECK-32-NEXT:   Demangled name: existentials.MyError

// CHECK-32: Start of instance data: [[ADDR:0x[0-9a-fA-F]+]]

reflectUnwrappingClassExistential(error: error)
// CHECK-64: Reflecting an error existential and unwrapping class.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-64: Type reference:
// CHECK-64: (struct existentials.MyError)

// CHECK-64:        Type info:
// CHECK-64:        (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:   (field name=i offset=0
// CHECK-64-NEXT:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:       (field name=_value offset=0
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1)))))
// CHECK-64-NEXT:   Mangled name: $s12existentials7MyErrorV
// CHECK-64-NEXT:   Demangled name: existentials.MyError

// CHECK-64: Start of instance data: 
// CHECK-64-NOT: ![[ADDR]]

// CHECK-32: Reflecting an error existential and unwrapping class.
// CHECK-32: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-32: Type reference:
// CHECK-32: (struct existentials.MyError)

// CHECK-32:        Type info:
// CHECK-32:        (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:   (field name=i offset=0
// CHECK-32-NEXT:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:       (field name=_value offset=0
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1)))))
// CHECK-32-NEXT:   Mangled name: $s12existentials7MyErrorV
// CHECK-32-NEXT:   Demangled name: existentials.MyError

// CHECK-32: Start of instance data: 
// CHECK-32-NOT: ![[ADDR]]

doneReflecting()
