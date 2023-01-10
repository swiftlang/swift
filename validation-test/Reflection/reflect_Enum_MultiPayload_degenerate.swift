// RUN: %empty-directory(%t)
// RUN: %target-build-swift -g -lswiftSwiftReflectionTest %s -o %t/reflect_Enum_MultiPayload_degenerate
// RUN: %target-codesign %t/reflect_Enum_MultiPayload_degenerate

// RUN: %target-run %target-swift-reflection-test %t/reflect_Enum_MultiPayload_degenerate | %FileCheck %s --check-prefix=CHECK-%target-ptrsize

// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib

import SwiftReflectionTest

// Only one case has a non-zero-sized payload, so this gets
// laid out the same as a single-payload enum
enum FooVoid {
case a([Int])
case b(Void)
}

reflect(enum: FooVoid.a([]))

// CHECK-64: Reflecting an enum.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-64: Type reference:
// CHECK-64: (enum reflect_Enum_MultiPayload_degenerate.FooVoid)

// CHECK-64: Type info:
// CHECK-64: (multi_payload_enum size=8 alignment=8 stride=8 num_extra_inhabitants=2147483646 bitwise_takable=1
// CHECK-64:   (case name=a index=0 offset=0
// CHECK-64:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=2147483647 bitwise_takable=1
// CHECK-64:       (field name=_buffer offset=0
// CHECK-64:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=2147483647 bitwise_takable=1
// CHECK-64:           (field name=_storage offset=0
// CHECK-64:             (struct size=8 alignment=8 stride=8 num_extra_inhabitants=2147483647 bitwise_takable=1
// CHECK-64:               (field name=rawValue offset=0
// CHECK-64:                 (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=2147483647 bitwise_takable=1))))))))
// CHECK-64:   (case name=b index=1 offset=0
// CHECK-64:     (tuple size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1)))
// CHECK-64: Mangled name: $s36reflect_Enum_MultiPayload_degenerate7FooVoidO
// CHECK-64: Demangled name: reflect_Enum_MultiPayload_degenerate.FooVoid

// CHECK-64: Enum value:
// CHECK-64: (enum_value name=a index=0
// CHECK-64: (bound_generic_struct Swift.Array
// CHECK-64:   (struct Swift.Int))
// CHECK-64: )

reflect(enum: FooVoid.b(()))

// CHECK-64: Reflecting an enum.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-64: Type reference:
// CHECK-64: (enum reflect_Enum_MultiPayload_degenerate.FooVoid)

// CHECK-64: Type info:
// CHECK-64: (multi_payload_enum size=8 alignment=8 stride=8 num_extra_inhabitants=2147483646 bitwise_takable=1
// CHECK-64:   (case name=a index=0 offset=0
// CHECK-64:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=2147483647 bitwise_takable=1
// CHECK-64:       (field name=_buffer offset=0
// CHECK-64:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=2147483647 bitwise_takable=1
// CHECK-64:           (field name=_storage offset=0
// CHECK-64:             (struct size=8 alignment=8 stride=8 num_extra_inhabitants=2147483647 bitwise_takable=1
// CHECK-64:               (field name=rawValue offset=0
// CHECK-64:                 (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=2147483647 bitwise_takable=1))))))))
// CHECK-64:   (case name=b index=1 offset=0
// CHECK-64:     (tuple size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1)))
// CHECK-64: Mangled name: $s36reflect_Enum_MultiPayload_degenerate7FooVoidO
// CHECK-64: Demangled name: reflect_Enum_MultiPayload_degenerate.FooVoid

// CHECK-64: Enum value:
// CHECK-64: (enum_value name=b index=1
// CHECK-64:   (tuple)
// CHECK-64: )


// Same as above, except the first payload has zero size
enum FooVoid2 {
case a(Void)
case b([Int])
}

reflect(enum: FooVoid2.a(()))

// CHECK-64: Reflecting an enum.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-64: Type reference:
// CHECK-64: (enum reflect_Enum_MultiPayload_degenerate.FooVoid2)

// CHECK-64: Type info:
// CHECK-64: (multi_payload_enum size=8 alignment=8 stride=8 num_extra_inhabitants=2147483646 bitwise_takable=1
// CHECK-64:   (case name=b index=0 offset=0
// CHECK-64:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=2147483647 bitwise_takable=1
// CHECK-64:       (field name=_buffer offset=0
// CHECK-64:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=2147483647 bitwise_takable=1
// CHECK-64:           (field name=_storage offset=0
// CHECK-64:             (struct size=8 alignment=8 stride=8 num_extra_inhabitants=2147483647 bitwise_takable=1
// CHECK-64:               (field name=rawValue offset=0
// CHECK-64:                 (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=2147483647 bitwise_takable=1))))))))
// CHECK-64:   (case name=a index=1 offset=0
// CHECK-64:     (tuple size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1)))
// CHECK-64: Mangled name: $s36reflect_Enum_MultiPayload_degenerate8FooVoid2O
// CHECK-64: Demangled name: reflect_Enum_MultiPayload_degenerate.FooVoid2

// CHECK-64: Enum value:
// CHECK-64: (enum_value name=a index=1
// CHECK-64:   (tuple)
// CHECK-64: )

reflect(enum: FooVoid2.b([]))

// CHECK-64: Reflecting an enum.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-64: Type reference:
// CHECK-64: (enum reflect_Enum_MultiPayload_degenerate.FooVoid2)

// CHECK-64: Type info:
// CHECK-64: (multi_payload_enum size=8 alignment=8 stride=8 num_extra_inhabitants=2147483646 bitwise_takable=1
// CHECK-64:   (case name=b index=0 offset=0
// CHECK-64:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=2147483647 bitwise_takable=1
// CHECK-64:       (field name=_buffer offset=0
// CHECK-64:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=2147483647 bitwise_takable=1
// CHECK-64:           (field name=_storage offset=0
// CHECK-64:             (struct size=8 alignment=8 stride=8 num_extra_inhabitants=2147483647 bitwise_takable=1
// CHECK-64:               (field name=rawValue offset=0
// CHECK-64:                 (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=2147483647 bitwise_takable=1))))))))
// CHECK-64:   (case name=a index=1 offset=0
// CHECK-64:     (tuple size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1)))
// CHECK-64: Mangled name: $s36reflect_Enum_MultiPayload_degenerate8FooVoid2O
// CHECK-64: Demangled name: reflect_Enum_MultiPayload_degenerate.FooVoid2

// CHECK-64: Enum value:
// CHECK-64: (enum_value name=b index=0
// CHECK-64: (bound_generic_struct Swift.Array
// CHECK-64:   (struct Swift.Int))
// CHECK-64: )


// As above, this is laid out as a single-payload enum
// because the `b` case payload has zero size
struct B {}
enum FooEmptyStruct {
case a([Int])
case b(B)
}

reflect(enum: FooEmptyStruct.a([]))

// CHECK-64: Reflecting an enum.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-64: Type reference:
// CHECK-64: (enum reflect_Enum_MultiPayload_degenerate.FooEmptyStruct)

// CHECK-64: Type info:
// CHECK-64: (multi_payload_enum size=8 alignment=8 stride=8 num_extra_inhabitants=2147483646 bitwise_takable=1
// CHECK-64:   (case name=a index=0 offset=0
// CHECK-64:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=2147483647 bitwise_takable=1
// CHECK-64:       (field name=_buffer offset=0
// CHECK-64:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=2147483647 bitwise_takable=1
// CHECK-64:           (field name=_storage offset=0
// CHECK-64:             (struct size=8 alignment=8 stride=8 num_extra_inhabitants=2147483647 bitwise_takable=1
// CHECK-64:               (field name=rawValue offset=0
// CHECK-64:                 (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=2147483647 bitwise_takable=1))))))))
// CHECK-64:   (case name=b index=1 offset=0
// CHECK-64:     (struct size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1)))
// CHECK-64: Mangled name: $s36reflect_Enum_MultiPayload_degenerate14FooEmptyStructO
// CHECK-64: Demangled name: reflect_Enum_MultiPayload_degenerate.FooEmptyStruct

// CHECK-64: Enum value:
// CHECK-64: (enum_value name=a index=0
// CHECK-64: (bound_generic_struct Swift.Array
// CHECK-64:   (struct Swift.Int))
// CHECK-64: )

reflect(enum: FooEmptyStruct.b(B()))

// CHECK-64: Reflecting an enum.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-64: Type reference:
// CHECK-64: (enum reflect_Enum_MultiPayload_degenerate.FooEmptyStruct)

// CHECK-64: Type info:
// CHECK-64: (multi_payload_enum size=8 alignment=8 stride=8 num_extra_inhabitants=2147483646 bitwise_takable=1
// CHECK-64:   (case name=a index=0 offset=0
// CHECK-64:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=2147483647 bitwise_takable=1
// CHECK-64:       (field name=_buffer offset=0
// CHECK-64:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=2147483647 bitwise_takable=1
// CHECK-64:           (field name=_storage offset=0
// CHECK-64:             (struct size=8 alignment=8 stride=8 num_extra_inhabitants=2147483647 bitwise_takable=1
// CHECK-64:               (field name=rawValue offset=0
// CHECK-64:                 (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=2147483647 bitwise_takable=1))))))))
// CHECK-64:   (case name=b index=1 offset=0
// CHECK-64:     (struct size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1)))
// CHECK-64: Mangled name: $s36reflect_Enum_MultiPayload_degenerate14FooEmptyStructO
// CHECK-64: Demangled name: reflect_Enum_MultiPayload_degenerate.FooEmptyStruct

// CHECK-64: Enum value:
// CHECK-64: (enum_value name=b index=1
// CHECK-64:   (struct reflect_Enum_MultiPayload_degenerate.B)
// CHECK-64: )



// TODO: Variations of Foo where `b` payload is class, Bool

doneReflecting()

// CHECK-32: FAIL

// CHECK: Done.
