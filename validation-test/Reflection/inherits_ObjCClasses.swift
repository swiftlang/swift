// RUN: %empty-directory(%t)

// RUN: %clang %target-cc-options -isysroot %sdk -fobjc-arc %S/Inputs/ObjCClasses/ObjCClasses.m -c -o %t/ObjCClasses.o
// RUN: %target-build-swift -I %S/Inputs/ObjCClasses/ -lswiftSwiftReflectionTest %t/ObjCClasses.o %s -o %t/inherits_ObjCClasses
// RUN: %target-codesign %t/inherits_ObjCClasses

// RUN: %target-run %target-swift-reflection-test %t/inherits_ObjCClasses | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-ptrsize


// REQUIRES: objc_interop
// REQUIRES: executable_test

import simd
import ObjCClasses
import SwiftReflectionTest

//// FirstClass -- base class, has one word-sized ivar

// Variant A: 16 byte alignment
class FirstClassA : FirstClass {
  var xx: int4 = [1,2,3,4]
}

let firstClassA = FirstClassA()
reflect(object: firstClassA)

// CHECK-64: Type reference:
// CHECK-64: (class inherits_ObjCClasses.FirstClassA)

// CHECK-64: Type info:
// CHECK-64: (class_instance size=32 alignment=16 stride=32 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:   (field name=xx offset=16
// CHECK-64:     (struct size=16 alignment=16 stride=16 num_extra_inhabitants=0 bitwise_takable=1

// CHECK-32: Reflecting an object.
// CHECK-32: Type reference:
// CHECK-32: (class inherits_ObjCClasses.FirstClassA)

// CHECK-32: Type info:
// CHECK-32-NEXT: (class_instance size=32 alignment=16 stride=32 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:   (field name=xx offset=16
// CHECK-32-NEXT:     (struct size=16 alignment=16 stride=16 num_extra_inhabitants=0 bitwise_takable=1

// Variant B: word size alignment
class FirstClassB : FirstClass {
  var zz: Int = 0
}

let firstClassB = FirstClassB()
reflect(object: firstClassB)

// CHECK-64: Type reference:
// CHECK-64: (class inherits_ObjCClasses.FirstClassB)

// CHECK-64: Type info:
// CHECK-64: (class_instance size=24 alignment=8 stride=24 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:   (field name=zz offset=16
// CHECK-64:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:       (field name=_value offset=0
// CHECK-64:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1)))))

// CHECK-32: Reflecting an object.
// CHECK-32: Type reference:
// CHECK-32: (class inherits_ObjCClasses.FirstClassB)

// CHECK-32: Type info:
// CHECK-32-NEXT: (class_instance size=12 alignment=4 stride=12 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:   (field name=zz offset=8
// CHECK-32-NEXT:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:       (field name=_value offset=0
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1)))))

//// SecondClass -- base class, has two word-sized ivars

// Variant A: 16 byte alignment
class SecondClassA : SecondClass {
  var xx: int4 = [1,2,3,4]
}

let secondClassA = SecondClassA()
reflect(object: secondClassA)

// CHECK-64: Type reference:
// CHECK-64: (class inherits_ObjCClasses.SecondClassA)

// CHECK-64: Type info:
// CHECK-64: (class_instance size=48 alignment=16 stride=48 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:   (field name=xx offset=32
// CHECK-64:     (struct size=16 alignment=16 stride=16 num_extra_inhabitants=0 bitwise_takable=1

// CHECK-32: Reflecting an object.
// CHECK-32: Type reference:
// CHECK-32: (class inherits_ObjCClasses.SecondClassA)

// CHECK-32: Type info:
// CHECK-32-NEXT: (class_instance size=32 alignment=16 stride=32 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:   (field name=xx offset=16
// CHECK-32-NEXT:     (struct size=16 alignment=16 stride=16 num_extra_inhabitants=0 bitwise_takable=1

// Variant B: word size alignment
class SecondClassB : SecondClass {
  var zz: Int = 0
}

let secondClassB = SecondClassB()
reflect(object: secondClassB)

// CHECK-64: Type reference:
// CHECK-64: (class inherits_ObjCClasses.SecondClassB)

// CHECK-64: Type info:
// CHECK-64: (class_instance size=32 alignment=8 stride=32 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:   (field name=zz offset=24
// CHECK-64:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:       (field name=_value offset=0
// CHECK-64:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1)))))

// CHECK-32: Reflecting an object.
// CHECK-32: Type reference:
// CHECK-32: (class inherits_ObjCClasses.SecondClassB)

// CHECK-32: Type info:
// CHECK-32-NEXT: (class_instance size=16 alignment=4 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:   (field name=zz offset=12
// CHECK-32-NEXT:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:       (field name=_value offset=0
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1)))))

//// ThirdClass -- base class, has three word-sized ivars

// Variant A: 16 byte alignment
class ThirdClassA : ThirdClass {
  var xx: int4 = [1,2,3,4]
}

let thirdClassA = ThirdClassA()
reflect(object: thirdClassA)

// CHECK-64: Type reference:
// CHECK-64: (class inherits_ObjCClasses.ThirdClassA)

// CHECK-64: Type info:
// CHECK-64: (class_instance size=48 alignment=16 stride=48 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:   (field name=xx offset=32
// CHECK-64:     (struct size=16 alignment=16 stride=16 num_extra_inhabitants=0 bitwise_takable=1

// CHECK-32: Reflecting an object.
// CHECK-32: Type reference:
// CHECK-32: (class inherits_ObjCClasses.ThirdClassA)

// CHECK-32: Type info:
// CHECK-32-NEXT: (class_instance size=32 alignment=16 stride=32 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:   (field name=xx offset=16
// CHECK-32-NEXT:     (struct size=16 alignment=16 stride=16 num_extra_inhabitants=0 bitwise_takable=1

// Variant B: word size alignment
class ThirdClassB : ThirdClass {
  var zz: Int = 0
}

let thirdClassB = ThirdClassB()
reflect(object: thirdClassB)

// CHECK-64: Type reference:
// CHECK-64: (class inherits_ObjCClasses.ThirdClassB)

// CHECK-64: Type info:
// CHECK-64: (class_instance size=40 alignment=8 stride=40 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:   (field name=zz offset=32
// CHECK-64:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:       (field name=_value offset=0
// CHECK-64:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1)))))

// CHECK-32: Reflecting an object.
// CHECK-32: Type reference:
// CHECK-32: (class inherits_ObjCClasses.ThirdClassB)

// CHECK-32: Type info:
// CHECK-32-NEXT: (class_instance size=20 alignment=4 stride=20 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:   (field name=zz offset=16
// CHECK-32-NEXT:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:       (field name=_value offset=0
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1)))))

doneReflecting()
