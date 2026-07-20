// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_rawLayout -target %module-target-future -enable-experimental-feature RawLayout
// RUN: %target-codesign %t/reflect_rawLayout

// RUN: %target-run %target-swift-reflection-test %t/reflect_rawLayout | %FileCheck %s --check-prefix=CHECK-%target-ptrsize

// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// REQUIRES: swift_feature_RawLayout
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: asan

// A `@_rawLayout(like:)` type has no stored properties, so without the
// artificial "like" field in its reflection metadata, remote reflection used
// to compute its size as zero and lay out following fields on top of it.
// rdar://180673451

import SwiftReflectionTest

@_rawLayout(like: T)
struct RawCell<T: ~Copyable>: ~Copyable {}

class TestClass {
    let before: Int = 1
    let cell: RawCell<Int> = RawCell()
    let after: Int = 2
}

let obj = TestClass()

reflect(object: obj)

// The raw-layout field must report the size and stride of its `like` type
// (Int) rather than zero, and the following field must not overlap it.

// CHECK-64: Reflecting an object.
// CHECK-64: Type reference:
// CHECK-64: (class reflect_rawLayout.TestClass)

// CHECK-64: Type info:
// CHECK-64: (class_instance size=40 alignment=8 stride=40 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:   (field name=before offset=16
// CHECK-64:   (field name=cell offset=24
// CHECK-64:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:       (field name=_rawLayout offset=0
// CHECK-64:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:           (field name=_value offset=0
// CHECK-64:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-64:   (field name=after offset=32

// CHECK-32: Reflecting an object.
// CHECK-32: Type reference:
// CHECK-32: (class reflect_rawLayout.TestClass)

// CHECK-32: Type info:
// CHECK-32: (class_instance
// CHECK-32:   (field name=before offset={{[0-9]+}}
// CHECK-32:   (field name=cell offset=[[CELLOFF:[0-9]+]]
// CHECK-32:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:       (field name=_rawLayout offset=0
// CHECK-32:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:           (field name=_value offset=0
// CHECK-32:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))))
// CHECK-32:   (field name=after offset={{[0-9]+}}

// A `@_rawLayout` type has no extra inhabitants, even when its `like` type
// does. Optional therefore cannot pack its tag into the payload and must add a
// separate tag byte. Remote reflection must not inherit the `like` type's extra
// inhabitants through the artificial field, or it would compute a too-small
// Optional and lay the following field on top of the tag.
class TestClassOptional {
  let before = 1
  let cell: RawCell<UnsafePointer<UInt8>>? = nil
  let after = 2
}

reflect(object: TestClassOptional())

// CHECK-64: Reflecting an object.
// CHECK-64: Type reference:
// CHECK-64: (class reflect_rawLayout.TestClassOptional)
// CHECK-64: Type info:
// CHECK-64: (class_instance size=48 alignment=8 stride=48 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:   (field name=before offset=16
// CHECK-64:   (field name=cell offset=24
// CHECK-64:     (single_payload_enum size=9 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:       (case name=some index=0 offset=0
// CHECK-64:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64:           (field name=_rawLayout offset=0
// CHECK-64:             (struct size=8 alignment=8 stride=8 num_extra_inhabitants=1 bitwise_takable=1
// CHECK-64:               (field name=_rawValue offset=0
// CHECK-64:                 (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1 bitwise_takable=1))))))
// CHECK-64:       (case name=none index=1)))
// CHECK-64:   (field name=after offset=40

// CHECK-32: Reflecting an object.
// CHECK-32: Type reference:
// CHECK-32: (class reflect_rawLayout.TestClassOptional)
// CHECK-32: Type info:
// CHECK-32: (class_instance
// CHECK-32:   (field name=before offset={{[0-9]+}}
// CHECK-32:   (field name=cell offset={{[0-9]+}}
// CHECK-32:     (single_payload_enum {{.*}}num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:       (case name=some index=0 offset=0
// CHECK-32:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32:           (field name=_rawLayout offset=0
// CHECK-32:             (struct {{.*}}bitwise_takable=1
// CHECK-32:               (field name=_rawValue offset=0
// CHECK-32:                 (builtin {{.*}})))))
// CHECK-32:       (case name=none index=1)))
// CHECK-32:   (field name=after offset={{[0-9]+}}

doneReflecting()

// CHECK-64: Done.
// CHECK-32: Done.
