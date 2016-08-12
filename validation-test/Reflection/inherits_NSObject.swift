// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/inherits_NSObject
// RUN: %target-run %target-swift-reflection-test %t/inherits_NSObject | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-ptrsize

// REQUIRES: objc_interop
// REQUIRES: executable_test

import Foundation

import SwiftReflectionTest

class InheritsNSObject : NSObject {}
class ContainsInheritsNSObject : NSObject {
    var a: InheritsNSObject
    var _b: InheritsNSObject

    var b: InheritsNSObject {
        get { return _b }
        set (newVal) { _b = newVal }
    }

    override init() {
        a = InheritsNSObject()
        _b = InheritsNSObject()
    }
}

let inheritsNSObject = InheritsNSObject()
reflect(object: inheritsNSObject)

// CHECK-64: Reflecting an object.
// CHECK-64: Type reference:
// CHECK-64: (class inherits_NSObject.InheritsNSObject)

// CHECK-64: Type info:
// CHECK-64: (class_instance size=8 alignment=16 stride=16 num_extra_inhabitants=0)

// CHECK-32: Reflecting an object.
// CHECK-32: Type reference:
// CHECK-32: (class inherits_NSObject.InheritsNSObject)

// CHECK-32: Type info:
// CHECK-32: (class_instance size=4 alignment=16 stride=16 num_extra_inhabitants=0)

let sc = ContainsInheritsNSObject()
reflect(object: sc)

// CHECK-64: Reflecting an object.
// CHECK-64: Type reference:
// CHECK-64: (class inherits_NSObject.ContainsInheritsNSObject)

// CHECK-64:        Type info:
// CHECK-64:        (class_instance size=24 alignment=16 stride=32 num_extra_inhabitants=0
// CHECK-64-NEXT:   (field name=a offset=8
// CHECK-64-NEXT:     (reference kind=strong refcounting=unknown))
// CHECK-64-NEXT:   (field name=_b offset=16
// CHECK-64-NEXT:     (reference kind=strong refcounting=unknown)))

// CHECK-32: Reflecting an object.
// CHECK-32: Type reference:
// CHECK-32: (class inherits_NSObject.ContainsInheritsNSObject)

// CHECK-32:        Type info:
// CHECK-32:        (class_instance size=12 alignment=16 stride=16 num_extra_inhabitants=0
// CHECK-32-NEXT:   (field name=a offset=4
// CHECK-32-NEXT:     (reference kind=strong refcounting=unknown))
// CHECK-32-NEXT:   (field name=_b offset=8
// CHECK-32-NEXT:     (reference kind=strong refcounting=unknown)))

doneReflecting()
