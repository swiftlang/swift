// RUN: %empty-directory(%t)

// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/functions
// RUN: %target-codesign %t/functions

// RUN: %target-run %target-swift-reflection-test %t/functions | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-ptrsize

// REQUIRES: objc_interop
// REQUIRES: executable_test

import SwiftReflectionTest
import Foundation
import CoreGraphics

func capturesImportedTypes(x: Int, n: NSURL, r: CGRect, c: NSCoding) {
  reflect(function: {print(x); print(n); print(r); print(c)})

// CHECK-32:      Type reference:
// CHECK-32-NEXT: (builtin Builtin.NativeObject)

// CHECK-32:      Type info:
// CHECK-32-NEXT: (closure_context size=36 alignment=4 stride=36 num_extra_inhabitants=0
// CHECK-32-NEXT:   (field offset=8
// CHECK-32-NEXT:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0
// CHECK-32-NEXT:       (field name=_value offset=0
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0))))
// CHECK-32-NEXT:   (field offset=12
// CHECK-32-NEXT:     (reference kind=strong refcounting=unknown))
// CHECK-32-NEXT:   (field offset=16
// CHECK-32-NEXT:     (builtin size=16 alignment=4 stride=16 num_extra_inhabitants=0))
// CHECK-32-NEXT:   (field offset=32
// CHECK-32-NEXT:     (reference kind=strong refcounting=unknown)))

// CHECK-64:      Type reference:
// CHECK-64-NEXT: (builtin Builtin.NativeObject)

// CHECK-64:      Type info:
// CHECK-64-NEXT: (closure_context size=72 alignment=8 stride=72 num_extra_inhabitants=0
// CHECK-64-NEXT:   (field offset=16
// CHECK-64-NEXT:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0
// CHECK-64-NEXT:       (field name=_value offset=0
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0))))
// CHECK-64-NEXT:   (field offset=24
// CHECK-64-NEXT:     (reference kind=strong refcounting=unknown))
// CHECK-64-NEXT:   (field offset=32
// CHECK-64-NEXT:     (builtin size=32 alignment=8 stride=32 num_extra_inhabitants=0))
// CHECK-64-NEXT:   (field offset=64
// CHECK-64-NEXT:     (reference kind=strong refcounting=unknown)))
}

capturesImportedTypes(x: 10, n: NSURL(), r: CGRect(x: 1, y: 2, width: 3, height: 4), c: "" as NSString)

doneReflecting()
