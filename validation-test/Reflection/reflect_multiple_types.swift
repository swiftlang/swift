// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_multiple_types
// RUN: %target-run %target-swift-reflection-test %t/reflect_multiple_types 2>&1 | %FileCheck %s --check-prefix=CHECK-%target-ptrsize
// REQUIRES: objc_interop
// REQUIRES: executable_test

// FIXME: https://bugs.swift.org/browse/SR-2808
// XFAIL: resilient_stdlib

import SwiftReflectionTest
import Foundation

class TestClass {
    var t00: Array<Int>
    var t01: Bool
    var t02: Character
    var t03: Dictionary<Int, Int>
    var t04: Double
    var t05: Float
    var t06: Int
    var t07: Int16
    var t08: Int32
    var t09: Int64
    var t10: Int8
    var t11: NSArray
    var t12: NSNumber
    var t13: NSSet
    var t14: NSString
    var t15: Set<Int>
    var t16: String
    var t17: UInt
    var t18: UInt16
    var t19: UInt32
    var t20: UInt64
    var t21: UInt8
    init(
        t00: Array<Int>,
        t01: Bool,
        t02: Character,
        t03: Dictionary<Int, Int>,
        t04: Double,
        t05: Float,
        t06: Int,
        t07: Int16,
        t08: Int32,
        t09: Int64,
        t10: Int8,
        t11: NSArray,
        t12: NSNumber,
        t13: NSSet,
        t14: NSString,
        t15: Set<Int>,
        t16: String,
        t17: UInt,
        t18: UInt16,
        t19: UInt32,
        t20: UInt64,
        t21: UInt8
    ) {
        self.t00 = t00
        self.t01 = t01
        self.t02 = t02
        self.t03 = t03
        self.t04 = t04
        self.t05 = t05
        self.t06 = t06
        self.t07 = t07
        self.t08 = t08
        self.t09 = t09
        self.t10 = t10
        self.t11 = t11
        self.t12 = t12
        self.t13 = t13
        self.t14 = t14
        self.t15 = t15
        self.t16 = t16
        self.t17 = t17
        self.t18 = t18
        self.t19 = t19
        self.t20 = t20
        self.t21 = t21
    }
}

var obj = TestClass(
    t00: [1, 2, 3],
    t01: true,
    t02: "A",
    t03: [1: 3, 2: 2, 3: 1],
    t04: 123.45,
    t05: 123.45,
    t06: 123,
    t07: 123,
    t08: 123,
    t09: 123,
    t10: 123,
    t11: [1, 2, 3],
    t12: 123,
    t13: [1, 2, 3, 3, 2, 1],
    t14: "Hello, NSString!",
    t15: [1, 2, 3, 3, 2, 1],
    t16: "Hello, Reflection!",
    t17: 123,
    t18: 123,
    t19: 123,
    t20: 123,
    t21: 123
)

reflect(object: obj)

// CHECK-64: Reflecting an object.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-64: Type reference:
// CHECK-64: (class reflect_multiple_types.TestClass)

// CHECK-64: Type info:
// CHECK-64: (class_instance size=193 alignment=8 stride=200 num_extra_inhabitants=0
// CHECK-64:   (field name=t00 offset=16
// CHECK-64:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=1
// (unstable implementation details omitted)
// CHECK-64:   (field name=t01 offset=24
// CHECK-64:     (struct size=1 alignment=1 stride=1 num_extra_inhabitants=254
// CHECK-64:       (field name=_value offset=0
// CHECK-64:         (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=254))))
// CHECK-64:   (field name=t02 offset=32
// CHECK-64:     (struct size=9 alignment=8 stride=16 num_extra_inhabitants=0
// CHECK-64:       (field name=_representation offset=0
// CHECK-64:         (multi_payload_enum size=9 alignment=8 stride=16 num_extra_inhabitants=0
// CHECK-64:           (field name=large offset=0
// CHECK-64:             (struct size=8 alignment=8 stride=8 num_extra_inhabitants=2147483646
// CHECK-64:               (field name=_storage offset=0
// CHECK-64:                 (single_payload_enum size=8 alignment=8 stride=8 num_extra_inhabitants=2147483646
// CHECK-64:                   (field name=some offset=0
// CHECK-64:                     (reference kind=strong refcounting=native))))))
// CHECK-64:           (field name=small offset=0
// CHECK-64:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=2147483647))))))
// CHECK-64:   (field name=t03 offset=48
// CHECK-64:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0
// (unstable implementation details omitted)
// CHECK-64:   (field name=t04 offset=56
// CHECK-64:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0
// CHECK-64:       (field name=_value offset=0
// CHECK-64:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0))))
// CHECK-64:   (field name=t05 offset=64
// CHECK-64:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0
// CHECK-64:       (field name=_value offset=0
// CHECK-64:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0))))
// CHECK-64:   (field name=t06 offset=72
// CHECK-64:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0
// CHECK-64:       (field name=_value offset=0
// CHECK-64:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0))))
// CHECK-64:   (field name=t07 offset=80
// CHECK-64:     (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0
// CHECK-64:       (field name=_value offset=0
// CHECK-64:         (builtin size=2 alignment=2 stride=2 num_extra_inhabitants=0))))
// CHECK-64:   (field name=t08 offset=84
// CHECK-64:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0
// CHECK-64:       (field name=_value offset=0
// CHECK-64:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0))))
// CHECK-64:   (field name=t09 offset=88
// CHECK-64:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0
// CHECK-64:       (field name=_value offset=0
// CHECK-64:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0))))
// CHECK-64:   (field name=t10 offset=96
// CHECK-64:     (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0
// CHECK-64:       (field name=_value offset=0
// CHECK-64:         (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0))))
// CHECK-64:   (field name=t11 offset=104
// CHECK-64:     (reference kind=strong refcounting=unknown))
// CHECK-64:   (field name=t12 offset=112
// CHECK-64:     (reference kind=strong refcounting=unknown))
// CHECK-64:   (field name=t13 offset=120
// CHECK-64:     (reference kind=strong refcounting=unknown))
// CHECK-64:   (field name=t14 offset=128
// CHECK-64:     (reference kind=strong refcounting=unknown))
// CHECK-64:   (field name=t15 offset=136
// CHECK-64:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0
// (unstable implementation details omitted)
// CHECK-64:   (field name=t16 offset=144
// CHECK-64:     (struct size=24 alignment=8 stride=24 num_extra_inhabitants=1
// (unstable implementation details omitted)
// CHECK-64:   (field name=t17 offset=168
// CHECK-64:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0
// CHECK-64:       (field name=_value offset=0
// CHECK-64:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0))))
// CHECK-64:   (field name=t18 offset=176
// CHECK-64:     (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0
// CHECK-64:       (field name=_value offset=0
// CHECK-64:         (builtin size=2 alignment=2 stride=2 num_extra_inhabitants=0))))
// CHECK-64:   (field name=t19 offset=180
// CHECK-64:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0
// CHECK-64:       (field name=_value offset=0
// CHECK-64:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0))))
// CHECK-64:   (field name=t20 offset=184
// CHECK-64:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0
// CHECK-64:       (field name=_value offset=0
// CHECK-64:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0))))
// CHECK-64:   (field name=t21 offset=192
// CHECK-64:     (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0
// CHECK-64:       (field name=_value offset=0
// CHECK-64:         (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0)))))

// CHECK-32: Reflecting an object.
// CHECK-32: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-32: Type reference:
// CHECK-32: (class reflect_multiple_types.TestClass)

// CHECK-32: Type info:
// CHECK-32: (class_instance size=129 alignment=8 stride=136 num_extra_inhabitants=0
// CHECK-32:   (field name=t00 offset=12
// CHECK-32:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=1
// (unstable implementation details omitted)
// CHECK-32:   (field name=t01 offset=16
// CHECK-32:     (struct size=1 alignment=1 stride=1 num_extra_inhabitants=254
// CHECK-32:       (field name=_value offset=0
// CHECK-32:         (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=254))))
// CHECK-32:   (field name=t02 offset=24
// CHECK-32:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0
// CHECK-32:       (field name=_representation offset=0
// CHECK-32:         (multi_payload_enum size=8 alignment=8 stride=8 num_extra_inhabitants=0
// CHECK-32:           (field name=large offset=0
// CHECK-32:             (struct size=4 alignment=4 stride=4 num_extra_inhabitants=4095
// CHECK-32:               (field name=_storage offset=0
// CHECK-32:                 (single_payload_enum size=4 alignment=4 stride=4 num_extra_inhabitants=4095
// CHECK-32:                   (field name=some offset=0
// CHECK-32:                     (reference kind=strong refcounting=native))))))
// CHECK-32:           (field name=small offset=0
// CHECK-32:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=2147483647))))))
// CHECK-32:   (field name=t03 offset=32
// CHECK-32:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0
// (unstable implementation details omitted)
// CHECK-32:   (field name=t04 offset=40
// CHECK-32:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0
// CHECK-32:       (field name=_value offset=0
// CHECK-32:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0))))
// CHECK-32:   (field name=t05 offset=48
// CHECK-32:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0
// CHECK-32:       (field name=_value offset=0
// CHECK-32:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0))))
// CHECK-32:   (field name=t06 offset=52
// CHECK-32:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0
// CHECK-32:       (field name=_value offset=0
// CHECK-32:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0))))
// CHECK-32:   (field name=t07 offset=56
// CHECK-32:     (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0
// CHECK-32:       (field name=_value offset=0
// CHECK-32:         (builtin size=2 alignment=2 stride=2 num_extra_inhabitants=0))))
// CHECK-32:   (field name=t08 offset=60
// CHECK-32:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0
// CHECK-32:       (field name=_value offset=0
// CHECK-32:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0))))
// CHECK-32:   (field name=t09 offset=64
// CHECK-32:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0
// CHECK-32:       (field name=_value offset=0
// CHECK-32:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0))))
// CHECK-32:   (field name=t10 offset=72
// CHECK-32:     (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0
// CHECK-32:       (field name=_value offset=0
// CHECK-32:         (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0))))
// CHECK-32:   (field name=t11 offset=76
// CHECK-32:     (reference kind=strong refcounting=unknown))
// CHECK-32:   (field name=t12 offset=80
// CHECK-32:     (reference kind=strong refcounting=unknown))
// CHECK-32:   (field name=t13 offset=84
// CHECK-32:     (reference kind=strong refcounting=unknown))
// CHECK-32:   (field name=t14 offset=88
// CHECK-32:     (reference kind=strong refcounting=unknown))
// CHECK-32:   (field name=t15 offset=92
// CHECK-32:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0
// (unstable implementation details omitted)
// CHECK-32:   (field name=t16 offset=96
// CHECK-32:     (struct size=12 alignment=4 stride=12 num_extra_inhabitants=1
// (unstable implementation details omitted)
// CHECK-32:   (field name=t17 offset=108
// CHECK-32:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0
// CHECK-32:       (field name=_value offset=0
// CHECK-32:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0))))
// CHECK-32:   (field name=t18 offset=112
// CHECK-32:     (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0
// CHECK-32:       (field name=_value offset=0
// CHECK-32:         (builtin size=2 alignment=2 stride=2 num_extra_inhabitants=0))))
// CHECK-32:   (field name=t19 offset=116
// CHECK-32:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0
// CHECK-32:       (field name=_value offset=0
// CHECK-32:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0))))
// CHECK-32:   (field name=t20 offset=120
// CHECK-32:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0
// CHECK-32:       (field name=_value offset=0
// CHECK-32:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0))))
// CHECK-32:   (field name=t21 offset=128
// CHECK-32:     (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0
// CHECK-32:       (field name=_value offset=0
// CHECK-32:         (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0)))))

doneReflecting()

// CHECK-64: Done.

// CHECK-32: Done.
