// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/inherits_NSObject
// RUN: %target-codesign %t/inherits_NSObject

// RUN: %target-run %target-swift-reflection-test %t/inherits_NSObject | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-ptrsize

// REQUIRES: objc_interop
// REQUIRES: executable_test

import Foundation
import simd

import SwiftReflectionTest

class BaseNSClass : NSObject {
  var w: Int = 0
  var x: Bool = false
}

let baseClass = BaseNSClass()
reflect(object: baseClass)

// CHECK-64: Reflecting an object.
// CHECK-64: Type reference:
// CHECK-64: (class inherits_NSObject.BaseNSClass)

// CHECK-64: Type info:
// CHECK-64-NEXT: (class_instance size=17 alignment=8 stride=24 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:   (field name=w offset=8
// CHECK-64-NEXT:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:       (field name=_value offset=0
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-64-NEXT:   (field name=x offset=16
// CHECK-64-NEXT:     (struct size=1 alignment=1 stride=1 num_extra_inhabitants=254 bitwise_takable=1
// CHECK-64-NEXT:       (field name=_value offset=0
// CHECK-64-NEXT:         (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=254 bitwise_takable=1)))))

// CHECK-32: Reflecting an object.
// CHECK-32: Type reference:
// CHECK-32: (class inherits_NSObject.BaseNSClass)

// CHECK-32: Type info:
// CHECK-32-NEXT: (class_instance size=9 alignment=4 stride=12 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:   (field name=w offset=4
// CHECK-32-NEXT:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:       (field name=_value offset=0
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-32-NEXT:   (field name=x offset=8
// CHECK-32-NEXT:     (struct size=1 alignment=1 stride=1 num_extra_inhabitants=254 bitwise_takable=1
// CHECK-32-NEXT:       (field name=_value offset=0
// CHECK-32-NEXT:         (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=254 bitwise_takable=1)))))

class DerivedNSClass : BaseNSClass {
  var y: Bool = false
  var z: Int = 0
}

let derivedClass = DerivedNSClass()
reflect(object: derivedClass)

// CHECK-64: Reflecting an object.
// CHECK-64: Type reference:
// CHECK-64: (class inherits_NSObject.DerivedNSClass)

// CHECK-64: Type info:
// CHECK-64-NEXT: (class_instance size=32 alignment=8 stride=32 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:   (field name=y offset=17
// CHECK-64-NEXT:     (struct size=1 alignment=1 stride=1 num_extra_inhabitants=254 bitwise_takable=1
// CHECK-64-NEXT:       (field name=_value offset=0
// CHECK-64-NEXT:         (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=254 bitwise_takable=1))))
// CHECK-64-NEXT:   (field name=z offset=24
// CHECK-64-NEXT:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:       (field name=_value offset=0
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1)))))

// CHECK-32: Reflecting an object.
// CHECK-32: Type reference:
// CHECK-32: (class inherits_NSObject.DerivedNSClass)

// CHECK-32: Type info:
// CHECK-32-NEXT: (class_instance size=16 alignment=4 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:   (field name=y offset=9
// CHECK-32-NEXT:     (struct size=1 alignment=1 stride=1 num_extra_inhabitants=254 bitwise_takable=1
// CHECK-32-NEXT:       (field name=_value offset=0
// CHECK-32-NEXT:         (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=254 bitwise_takable=1))))
// CHECK-32-NEXT:   (field name=z offset=12
// CHECK-32-NEXT:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:       (field name=_value offset=0
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1)))))

// Note: dynamic layout starts at offset 8, not 16
class GenericBaseNSClass<T> : NSObject {
  var w: T = 0 as! T
}

let genericBaseClass = GenericBaseNSClass<Int>()
reflect(object: genericBaseClass)

// CHECK-64: Reflecting an object.
// CHECK-64: Type reference:
// CHECK-64: (bound_generic_class inherits_NSObject.GenericBaseNSClass
// CHECK-64:   (struct Swift.Int))

// CHECK-64: Type info:
// CHECK-64-NEXT: (class_instance size=16 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:   (field name=w offset=8
// CHECK-64-NEXT:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:       (field name=_value offset=0
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1)))))

// CHECK-32: Reflecting an object.
// CHECK-32: Type reference:
// CHECK-32: (bound_generic_class inherits_NSObject.GenericBaseNSClass
// CHECK-32:   (struct Swift.Int))

// CHECK-32: Type info:
// CHECK-32-NEXT: (class_instance size=8 alignment=4 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:   (field name=w offset=4
// CHECK-32-NEXT:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:       (field name=_value offset=0
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1)))))

class AlignedNSClass : NSObject {
  var w: Int = 0
  var x: int4 = [1,2,3,4]
}

let alignedClass = AlignedNSClass()
reflect(object: alignedClass)

// CHECK-64: Reflecting an object.
// CHECK-64: Type reference:
// CHECK-64: (class inherits_NSObject.AlignedNSClass)

// CHECK-64: Type info:
// CHECK-64-NEXT: (class_instance size=32 alignment=16 stride=32 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:   (field name=w offset=8
// CHECK-64-NEXT:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:       (field name=_value offset=0
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-64-NEXT:   (field name=x offset=16
// CHECK-64-NEXT:     (struct size=16 alignment=16 stride=16 num_extra_inhabitants=0 bitwise_takable=1

// CHECK-32: Reflecting an object.
// CHECK-32: Type reference:
// CHECK-32: (class inherits_NSObject.AlignedNSClass)

// CHECK-32: Type info:
// CHECK-32-NEXT: (class_instance size=32 alignment=16 stride=32 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:   (field name=w offset=4
// CHECK-32-NEXT:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:       (field name=_value offset=0
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-32-NEXT:   (field name=x offset=16
// CHECK-32-NEXT:     (struct size=16 alignment=16 stride=16 num_extra_inhabitants=0 bitwise_takable=1

class GenericAlignedNSClass<T> : NSObject {
  var w: T = 0 as! T
  var x: int4 = [1,2,3,4]
}

let genericAlignedClass = GenericAlignedNSClass<Int>()
reflect(object: genericAlignedClass)

// CHECK-64: Reflecting an object.
// CHECK-64: Type reference:
// CHECK-64: (bound_generic_class inherits_NSObject.GenericAlignedNSClass
// CHECK-64:   (struct Swift.Int))

// CHECK-64: Type info:
// CHECK-64-NEXT: (class_instance size=48 alignment=16 stride=48 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:   (field name=w offset=16
// CHECK-64-NEXT:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:       (field name=_value offset=0
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-64-NEXT:   (field name=x offset=32
// CHECK-64-NEXT:     (struct size=16 alignment=16 stride=16 num_extra_inhabitants=0 bitwise_takable=1

// CHECK-32: Reflecting an object.
// CHECK-32: Type reference:
// CHECK-32: (bound_generic_class inherits_NSObject.GenericAlignedNSClass
// CHECK-32:   (struct Swift.Int))

// CHECK-32: Type info:
// CHECK-32-NEXT: (class_instance size=48 alignment=16 stride=48 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:   (field name=w offset=16
// CHECK-32-NEXT:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:       (field name=_value offset=0
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-32-NEXT:   (field name=x offset=32
// CHECK-32-NEXT:     (struct size=16 alignment=16 stride=16 num_extra_inhabitants=0 bitwise_takable=1

doneReflecting()
