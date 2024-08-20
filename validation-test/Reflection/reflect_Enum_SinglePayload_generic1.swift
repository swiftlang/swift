// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_Enum_SinglePayload_generic1
// RUN: %target-codesign %t/reflect_Enum_SinglePayload_generic1

// RUN: %target-run %target-swift-reflection-test %t/reflect_Enum_SinglePayload_generic1 | tee /dev/stderr | %FileCheck %s --check-prefix=CHECK --check-prefix=X%target-ptrsize %add_num_extra_inhabitants --dump-input=fail

// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan

import SwiftReflectionTest

class ClassTypeA {}
class ClassTypeB {}
enum SimplePayload1<T>{
case a
case b(T)
case c
}

reflect(enum: SimplePayload1<ClassTypeA>.b(ClassTypeA()))

// CHECK: Reflecting an enum.
// CHECK-NEXT: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (bound_generic_enum reflect_Enum_SinglePayload_generic1.SimplePayload1
// CHECK-NEXT:   (class reflect_Enum_SinglePayload_generic1.ClassTypeA))

// MemoryLayout<SimplePayload1<ClassTypeA>> gives 8,8,8
// So it is using the pointer for XIs
// (Unlike MPEs, SPEs do not automatically use a tag just because they're generic)

// CHECK: Type info:
// X64-NEXT: (single_payload_enum size=8 alignment=8 stride=8 num_extra_inhabitants=[[#num_extra_inhabitants_64bit-2]] bitwise_takable=1
// X32-NEXT: (single_payload_enum size=4 alignment=4 stride=4 num_extra_inhabitants={{[0-9]+}} bitwise_takable=1
// CHECK-NEXT:   (case name=b index=0 offset=0
// CHECK-NEXT:     (reference kind=strong refcounting=native))
// CHECK-NEXT:   (case name=a index=1)
// CHECK-NEXT:   (case name=c index=2))
// CHECK-NEXT: Mangled name: $s35reflect_Enum_SinglePayload_generic114SimplePayload1OyAA10ClassTypeACG
// CHECK-NEXT: Demangled name: reflect_Enum_SinglePayload_generic1.SimplePayload1<reflect_Enum_SinglePayload_generic1.ClassTypeA>

// CHECK: Enum value:
// CHECK-NEXT: (enum_value name=b index=0
// CHECK-NEXT: (class reflect_Enum_SinglePayload_generic1.ClassTypeA)
// CHECK-NEXT: )

reflect(enum: SimplePayload1<ClassTypeA>.a)

// CHECK: Enum value:
// CHECK-NEXT: (enum_value name=a index=1)

reflect(enum: SimplePayload1<ClassTypeA>.c)

// CHECK: Enum value:
// CHECK-NEXT: (enum_value name=c index=2)

doneReflecting()

// CHECK: Done.

