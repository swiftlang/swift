// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_Enum_MultiPayload_generic
// RUN: %target-codesign %t/reflect_Enum_MultiPayload_generic

// RUN: %target-run %target-swift-reflection-test %t/reflect_Enum_MultiPayload_generic | tee /dev/stderr | %FileCheck %s --check-prefix=CHECK --check-prefix=X%target-ptrsize --dump-input=fail

// REQUIRES: reflection_test_support
// REQUIRES: objc_interop
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib

import SwiftReflectionTest

struct S {
  var a: Int
  var b: Int
  var c: Int
}

class ClassWithEnumDepth0<T> {
  enum E {
  case t(T)
  case u(Int)
  }
  var e: E?
}

/*
print("ClassWithEnumDepth0:  ",
  MemoryLayout<ClassWithEnumDepth0<S>.E?>.size,
  " ",
  MemoryLayout<ClassWithEnumDepth0<S>.E?>.alignment,
  " ",
  MemoryLayout<ClassWithEnumDepth0<S>.E?>.stride)
*/

reflect(object: ClassWithEnumDepth0<S>())

// CHECK: Reflecting an object.
// CHECK-NEXT: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (bound_generic_class reflect_Enum_MultiPayload_generic.ClassWithEnumDepth0
// CHECK-NEXT:   (struct reflect_Enum_MultiPayload_generic.S))

// CHECK: Type info:

// X64-NEXT: (class_instance size=41 alignment=8 stride=48 num_extra_inhabitants=0 bitwise_takable=1
// X64-NEXT:   (field name=e offset=16
// X64-NEXT:     (single_payload_enum size=25 alignment=8 stride=32 num_extra_inhabitants=253 bitwise_takable=1
// X64-NEXT:       (case name=some index=0 offset=0
// X64-NEXT:         (multi_payload_enum size=25 alignment=8 stride=32 num_extra_inhabitants=254 bitwise_takable=1
// X64-NEXT:           (case name=t index=0 offset=0
// X64-NEXT:             (struct size=24 alignment=8 stride=24 num_extra_inhabitants=0 bitwise_takable=1
// X64-NEXT:               (field name=a offset=0
// X64-NEXT:                 (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// X64-NEXT:                   (field name=_value offset=0
// X64-NEXT:                     (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
// X64-NEXT:               (field name=b offset=8
// X64-NEXT:                 (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// X64-NEXT:                   (field name=_value offset=0
// X64-NEXT:                     (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
// X64-NEXT:               (field name=c offset=16
// X64-NEXT:                 (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// X64-NEXT:                   (field name=_value offset=0
// X64-NEXT:                     (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))))
// X64-NEXT:           (case name=u index=1 offset=0
// X64-NEXT:             (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// X64-NEXT:               (field name=_value offset=0
// X64-NEXT:                 (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))))
// X64-NEXT:       (case name=none index=1))))

// X32-NEXT: (class_instance size=21 alignment=4 stride=24 num_extra_inhabitants=0 bitwise_takable=1
// X32-NEXT:   (field name=e offset=8
// X32-NEXT:     (single_payload_enum size=13 alignment=4 stride=16 num_extra_inhabitants=253 bitwise_takable=1
// X32-NEXT:       (case name=some index=0 offset=0
// X32-NEXT:         (multi_payload_enum size=13 alignment=4 stride=16 num_extra_inhabitants=254 bitwise_takable=1
// X32-NEXT:           (case name=t index=0 offset=0
// X32-NEXT:             (struct size=12 alignment=4 stride=12 num_extra_inhabitants=0 bitwise_takable=1
// X32-NEXT:               (field name=a offset=0
// X32-NEXT:                 (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// X32-NEXT:                   (field name=_value offset=0
// X32-NEXT:                     (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))
// X32-NEXT:               (field name=b offset=4
// X32-NEXT:                 (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// X32-NEXT:                   (field name=_value offset=0
// X32-NEXT:                     (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))
// X32-NEXT:               (field name=c offset=8
// X32-NEXT:                 (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// X32-NEXT:                   (field name=_value offset=0
// X32-NEXT:                     (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))))
// X32-NEXT:           (case name=u index=1 offset=0
// X32-NEXT:             (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// X32-NEXT:               (field name=_value offset=0
// X32-NEXT:                 (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))))
// X32-NEXT:       (case name=none index=1))))

class ClassWithEnumDepth1<T> {
  enum E<T> {
  case t(T)
  case u(Int)
  }
  var e: E<T>?
}

reflect(object: ClassWithEnumDepth1<S>())

// CHECK: Reflecting an object.
// CHECK-NEXT: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (bound_generic_class reflect_Enum_MultiPayload_generic.ClassWithEnumDepth1
// CHECK-NEXT:   (struct reflect_Enum_MultiPayload_generic.S))

// CHECK: Type info:

// X64-NEXT: (class_instance size=41 alignment=8 stride=48 num_extra_inhabitants=0 bitwise_takable=1
// X64-NEXT:   (field name=e offset=16
// X64-NEXT:     (single_payload_enum size=25 alignment=8 stride=32 num_extra_inhabitants=253 bitwise_takable=1
// X64-NEXT:       (case name=some index=0 offset=0
// X64-NEXT:         (multi_payload_enum size=25 alignment=8 stride=32 num_extra_inhabitants=254 bitwise_takable=1
// X64-NEXT:           (case name=t index=0 offset=0
// X64-NEXT:             (struct size=24 alignment=8 stride=24 num_extra_inhabitants=0 bitwise_takable=1
// X64-NEXT:               (field name=a offset=0
// X64-NEXT:                 (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// X64-NEXT:                   (field name=_value offset=0
// X64-NEXT:                     (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
// X64-NEXT:               (field name=b offset=8
// X64-NEXT:                 (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// X64-NEXT:                   (field name=_value offset=0
// X64-NEXT:                     (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
// X64-NEXT:               (field name=c offset=16
// X64-NEXT:                 (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// X64-NEXT:                   (field name=_value offset=0
// X64-NEXT:                     (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))))
// X64-NEXT:           (case name=u index=1 offset=0
// X64-NEXT:             (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// X64-NEXT:               (field name=_value offset=0
// X64-NEXT:                 (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))))
// X64-NEXT:       (case name=none index=1))))

// X32-NEXT: (class_instance size=21 alignment=4 stride=24 num_extra_inhabitants=0 bitwise_takable=1
// X32-NEXT:   (field name=e offset=8
// X32-NEXT:     (single_payload_enum size=13 alignment=4 stride=16 num_extra_inhabitants=253 bitwise_takable=1
// X32-NEXT:       (case name=some index=0 offset=0
// X32-NEXT:         (multi_payload_enum size=13 alignment=4 stride=16 num_extra_inhabitants=254 bitwise_takable=1
// X32-NEXT:           (case name=t index=0 offset=0
// X32-NEXT:             (struct size=12 alignment=4 stride=12 num_extra_inhabitants=0 bitwise_takable=1
// X32-NEXT:               (field name=a offset=0
// X32-NEXT:                 (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// X32-NEXT:                   (field name=_value offset=0
// X32-NEXT:                     (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))
// X32-NEXT:               (field name=b offset=4
// X32-NEXT:                 (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// X32-NEXT:                   (field name=_value offset=0
// X32-NEXT:                     (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))
// X32-NEXT:               (field name=c offset=8
// X32-NEXT:                 (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// X32-NEXT:                   (field name=_value offset=0
// X32-NEXT:                     (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))))
// X32-NEXT:           (case name=u index=1 offset=0
// X32-NEXT:             (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// X32-NEXT:               (field name=_value offset=0
// X32-NEXT:                 (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))))
// X32-NEXT:       (case name=none index=1))))

doneReflecting()

// CHECK: Done.

