// RUN: %empty-directory(%t)
// RUN: %target-build-swift -g -lswiftSwiftReflectionTest %s -o %t/reflect_Enum_MultiPayload_degenerate
// RUN: %target-codesign %t/reflect_Enum_MultiPayload_degenerate

// RUN: %target-run %target-swift-reflection-test %t/reflect_Enum_MultiPayload_degenerate | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-ptrsize %add_num_extra_inhabitants

// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan

import SwiftReflectionTest

class C {}

// Only one case has a non-zero-sized payload, so this gets
// laid out the same as a single-payload enum
enum FooVoid {
case a(C)
case b(Void)
}

reflect(enum: FooVoid.a(C()))

// CHECK: Reflecting an enum.
// CHECK: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK: Type reference:
// CHECK: (enum reflect_Enum_MultiPayload_degenerate.FooVoid)

// CHECK: Type info:

// Note: MemoryLayout<FooVoid> says that this really is size=8, alignment=8, stride=8
// Explanation: C is a pointer, so this enum can use NULL to represent the other case
// Aside: In TypeLowering.cpp, enum FooVoid does not have a FixedDescriptor even though
// its not generic.  This is why we check for having only a single payload first.

// CHECK-64: (multi_payload_enum size=8 alignment=8 stride=8 num_extra_inhabitants=[[#num_extra_inhabitants_64bit-1]] bitwise_takable=1
// CHECK-64:   (case name=a index=0 offset=0
// CHECK-64:     (reference kind=strong refcounting=native))

// CHECK-32: (multi_payload_enum size=4 alignment=4 stride=4 num_extra_inhabitants=4095 bitwise_takable=1
// CHECK-32:   (case name=a index=0 offset=0
// CHECK-32:     (reference kind=strong refcounting=native))

// CHECK:   (case name=b index=1 offset=0
// CHECK:     (tuple size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1)))

// CHECK: Mangled name: $s36reflect_Enum_MultiPayload_degenerate7FooVoidO
// CHECK: Demangled name: reflect_Enum_MultiPayload_degenerate.FooVoid

// CHECK: Enum value:
// CHECK-NEXT: (enum_value name=a index=0
// CHECK-NEXT: (class reflect_Enum_MultiPayload_degenerate.C)
// CHECK-NEXT: )

reflect(enum: FooVoid.b(()))

// CHECK: Reflecting an enum.
// CHECK: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK: Type reference:
// CHECK: (enum reflect_Enum_MultiPayload_degenerate.FooVoid)

// CHECK: Type info:

// CHECK: Mangled name: $s36reflect_Enum_MultiPayload_degenerate7FooVoidO
// CHECK: Demangled name: reflect_Enum_MultiPayload_degenerate.FooVoid

// CHECK: Enum value:
// CHECK-NEXT: (enum_value name=b index=1
// CHECK-NEXT:   (tuple)
// CHECK-NEXT: )


// Same as above, except the first payload has zero size
enum FooVoid2 {
case a(Void)
case b(C)
}

reflect(enum: FooVoid2.a(()))

// CHECK: Reflecting an enum.
// CHECK: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK: Type reference:
// CHECK: (enum reflect_Enum_MultiPayload_degenerate.FooVoid2)

// CHECK: Type info:

// CHECK-64: (multi_payload_enum size=8 alignment=8 stride=8 num_extra_inhabitants=[[#num_extra_inhabitants_64bit-1]] bitwise_takable=1
// CHECK-64:   (case name=b index=0 offset=0
// CHECK-64:     (reference kind=strong refcounting=native))

// CHECK-32: (multi_payload_enum size=4 alignment=4 stride=4 num_extra_inhabitants=4095 bitwise_takable=1
// CHECK-32:   (case name=b index=0 offset=0
// CHECK-32:     (reference kind=strong refcounting=native))

// CHECK:   (case name=a index=1 offset=0
// CHECK:     (tuple size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1)))
// CHECK: Mangled name: $s36reflect_Enum_MultiPayload_degenerate8FooVoid2O
// CHECK: Demangled name: reflect_Enum_MultiPayload_degenerate.FooVoid2

// CHECK: Enum value:
// CHECK: (enum_value name=a index=1
// CHECK:   (tuple)
// CHECK: )

reflect(enum: FooVoid2.b(C()))

// CHECK: Reflecting an enum.

// CHECK: Type info:
// CHECK: Mangled name: $s36reflect_Enum_MultiPayload_degenerate8FooVoid2O
// CHECK: Demangled name: reflect_Enum_MultiPayload_degenerate.FooVoid2

// CHECK: Enum value:
// CHECK-NEXT: (enum_value name=b index=0
// CHECK-NEXT: (class reflect_Enum_MultiPayload_degenerate.C)
// CHECK-NEXT: )


// As above, this is laid out as a single-payload enum
// because the `b` case payload has zero size
struct B {}
enum FooEmptyStruct {
case a(C)
case b(B)
}

reflect(enum: FooEmptyStruct.a(C()))

// CHECK: Reflecting an enum.
// CHECK: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK: Type reference:
// CHECK: (enum reflect_Enum_MultiPayload_degenerate.FooEmptyStruct)

// CHECK: Type info:

// CHECK-64: (multi_payload_enum size=8 alignment=8 stride=8 num_extra_inhabitants=[[#num_extra_inhabitants_64bit-1]] bitwise_takable=1
// CHECK-64:   (case name=a index=0 offset=0
// CHECK-64:     (reference kind=strong refcounting=native))

// CHECK-32: (multi_payload_enum size=4 alignment=4 stride=4 num_extra_inhabitants=4095 bitwise_takable=1
// CHECK-32:   (case name=a index=0 offset=0
// CHECK-32:     (reference kind=strong refcounting=native))

// CHECK:   (case name=b index=1 offset=0
// CHECK:     (struct size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1)))
// CHECK: Mangled name: $s36reflect_Enum_MultiPayload_degenerate14FooEmptyStructO
// CHECK: Demangled name: reflect_Enum_MultiPayload_degenerate.FooEmptyStruct

// CHECK: Enum value:
// CHECK-NEXT: (enum_value name=a index=0
// CHECK-NEXT: (class reflect_Enum_MultiPayload_degenerate.C)
// CHECK-NEXT: )

reflect(enum: FooEmptyStruct.b(B()))

// CHECK: Reflecting an enum.
// CHECK: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK: Type reference:
// CHECK: (enum reflect_Enum_MultiPayload_degenerate.FooEmptyStruct)

// CHECK: Type info:

// CHECK: Mangled name: $s36reflect_Enum_MultiPayload_degenerate14FooEmptyStructO
// CHECK: Demangled name: reflect_Enum_MultiPayload_degenerate.FooEmptyStruct

// CHECK: Enum value:
// CHECK-NEXT: (enum_value name=b index=1
// CHECK-NEXT:   (struct reflect_Enum_MultiPayload_degenerate.B)
// CHECK-NEXT: )

// TODO: Variations of Foo where `b` payload is class, Bool

doneReflecting()

// CHECK: Done.
