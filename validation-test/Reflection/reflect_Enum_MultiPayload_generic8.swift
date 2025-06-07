// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_Enum_MultiPayload_generic8
// RUN: %target-codesign %t/reflect_Enum_MultiPayload_generic8

// RUN: %target-run %target-swift-reflection-test %t/reflect_Enum_MultiPayload_generic8 | tee /dev/stderr | %FileCheck %s --check-prefix=CHECK --check-prefix=X%target-ptrsize --dump-input=fail

// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan

import SwiftReflectionTest

struct S<T> { var t: T }

enum A<T> {
case a(S<T>)
case b(Int)
case c(Void)
}

reflect(enum: A<Void>.a(S<Void>(t: ())) as A<Void>?)

// CHECK: Reflecting an enum.
// CHECK-NEXT: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (bound_generic_enum Swift.Optional
// CHECK-NEXT:   (bound_generic_enum reflect_Enum_MultiPayload_generic8.A
// CHECK-NEXT:     (tuple)))

// Note: MemoryLayout<A<Void>?>.size == MemoryLayout<A<Void>>.size == 9
// This means the MPE is exporting XIs from the tag, which means
// it's an MPE layout, not an SPE layout.
// Explanation:  S<T> is generic, thus treated as a non-empty payload
// even though it is in fact zero-sized.

// CHECK: Type info:
// X64-NEXT: (single_payload_enum size=9 alignment=8 stride=16 num_extra_inhabitants=252 bitwise_takable=1
// X64-NEXT:   (case name=some index=0 offset=0
// X64-NEXT:     (multi_payload_enum size=9 alignment=8 stride=16 num_extra_inhabitants=253 bitwise_takable=1
// X64-NEXT:       (case name=a index=0 offset=0
// X64-NEXT:         (struct size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// X64-NEXT:           (field name=t offset=0
// X64-NEXT:             (tuple size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))))
// X64-NEXT:       (case name=b index=1 offset=0
// X64-NEXT:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// X64-NEXT:           (field name=_value offset=0
// X64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
// X64-NEXT:       (case name=c index=2 offset=0
// X64-NEXT:         (tuple size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))))
// X64-NEXT:   (case name=none index=1))

// CHECK: Mangled name: $s34reflect_Enum_MultiPayload_generic81AOyytGSg
// CHECK-NEXT: Demangled name: Swift.Optional<reflect_Enum_MultiPayload_generic8.A<()>>

// CHECK: Enum value:
// CHECK-NEXT: (enum_value name=some index=0
// CHECK-NEXT: (bound_generic_enum reflect_Enum_MultiPayload_generic8.A
// CHECK-NEXT:   (tuple))
// CHECK-NEXT: )

reflect(enum: A<Void>.a(S<Void>(t: ())))

// CHECK: Enum value:
// CHECK-NEXT: (enum_value name=a index=0
// CHECK-NEXT:   (bound_generic_struct reflect_Enum_MultiPayload_generic8.S
// CHECK-NEXT:     (tuple))
// CHECK-NEXT: )

reflect(enum: A<Void>?.none)

// CHECK: Enum value:
// CHECK-NEXT: (enum_value name=none index=1)

reflect(enum: A<Void>.c(()))

// CHECK: Enum value:
// CHECK-NEXT: (enum_value name=c index=2
// CHECK-NEXT:   (tuple)
// CHECK-NEXT: )


enum B<T> {
case a(S<Void>)
case b(S<T>)
case c(Void)
}

reflect(enum: B<Int>.a(S<Void>(t: ())) as B<Int>?)

// CHECK: Reflecting an enum.
// CHECK-NEXT: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (bound_generic_enum Swift.Optional
// CHECK-NEXT:   (bound_generic_enum reflect_Enum_MultiPayload_generic8.B
// CHECK-NEXT:     (struct Swift.Int)))

// CHECK: Type info:
// X64-NEXT: (single_payload_enum size=10 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// X64-NEXT:   (case name=some index=0 offset=0
// X64-NEXT:     (multi_payload_enum size=9 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// X64-NEXT:       (case name=b index=0 offset=0
// X64-NEXT:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// X64-NEXT:           (field name=t offset=0
// X64-NEXT:             (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// X64-NEXT:               (field name=_value offset=0
// X64-NEXT:                 (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))))
// X64-NEXT:       (case name=a index=1 offset=0
// X64-NEXT:         (struct size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// X64-NEXT:           (field name=t offset=0
// X64-NEXT:             (tuple size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))))
// X64-NEXT:       (case name=c index=2 offset=0
// X64-NEXT:         (tuple size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))))
// X64-NEXT:   (case name=none index=1))

// CHECK: Mangled name: $s34reflect_Enum_MultiPayload_generic81BOySiGSg
// CHECK-NEXT: Demangled name: Swift.Optional<reflect_Enum_MultiPayload_generic8.B<Swift.Int>>

// CHECK: Enum value:
// CHECK-NEXT: (enum_value name=some index=0
// CHECK-NEXT: (bound_generic_enum reflect_Enum_MultiPayload_generic8.B
// CHECK-NEXT:   (struct Swift.Int))
// CHECK-NEXT: )

reflect(enum: B<Int>?.none)

// CHECK: Enum value:
// CHECK-NEXT: (enum_value name=none index=1)

reflect(enum: B<Int>.a(S<Void>(t: ())))

// CHECK: Enum value:
// CHECK-NEXT: (enum_value name=a index=1
// CHECK-NEXT:   (bound_generic_struct reflect_Enum_MultiPayload_generic8.S
// CHECK-NEXT:     (tuple))
// CHECK-NEXT: )


reflect(enum: B<Int>.b(S<Int>(t: 17)))

// CHECK: Enum value:
// CHECK-NEXT: (enum_value name=b index=0
// CHECK-NEXT:   (bound_generic_struct reflect_Enum_MultiPayload_generic8.S
// CHECK-NEXT:     (struct Swift.Int))
// CHECK-NEXT: )

reflect(enum: B<Int>.c(()))

// CHECK: Enum value:
// CHECK-NEXT: (enum_value name=c index=2
// CHECK-NEXT:   (tuple)
// CHECK-NEXT: )

doneReflecting()

// CHECK: Done.

