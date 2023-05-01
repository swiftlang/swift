// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_Enum_MultiPayload_generic2
// RUN: %target-codesign %t/reflect_Enum_MultiPayload_generic2

// RUN: %target-run %target-swift-reflection-test %t/reflect_Enum_MultiPayload_generic2 | tee /dev/stderr | %FileCheck %s --check-prefix=CHECK --check-prefix=X%target-ptrsize --dump-input=fail

// REQUIRES: reflection_test_support
// REQUIRES: objc_interop
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib

import SwiftReflectionTest

class ClassTypeA {}
class ClassTypeB {}
enum SimplePayload1<T, U>{
case a(T)
case b(U)
}

reflect(enum: SimplePayload1<ClassTypeA, Void>.a(ClassTypeA()))

// CHECK: Reflecting an enum.
// CHECK-NEXT: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (bound_generic_enum reflect_Enum_MultiPayload_generic2.SimplePayload1
// CHECK-NEXT:   (class reflect_Enum_MultiPayload_generic2.ClassTypeA)
// CHECK-NEXT:   (tuple))

// MemoryLayout<SimplePayload1<ClassTypeA, Void>> gives 9,8,16

// CHECK: Type info:
// X64-NEXT: (multi_payload_enum size=9 alignment=8 stride=16 num_extra_inhabitants=254 bitwise_takable=1
// X64-NEXT:   (case name=a index=0 offset=0
// X64-NEXT:     (reference kind=strong refcounting=native))
// X64-NEXT:   (case name=b index=1 offset=0
// X64-NEXT:     (tuple size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1)))

// CHECK: Mangled name: $s34reflect_Enum_MultiPayload_generic214SimplePayload1OyAA10ClassTypeACytG
// CHECK-NEXT: Demangled name: reflect_Enum_MultiPayload_generic2.SimplePayload1<reflect_Enum_MultiPayload_generic2.ClassTypeA, ()>

// CHECK: Enum value:
// CHECK-NEXT: (enum_value name=a index=0
// CHECK-NEXT: (class reflect_Enum_MultiPayload_generic2.ClassTypeA)
// CHECK-NEXT: )


reflect(enum: SimplePayload1<ClassTypeA, Void>.b(()))

// CHECK: Reflecting an enum.
// CHECK-NEXT: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (bound_generic_enum reflect_Enum_MultiPayload_generic2.SimplePayload1
// CHECK-NEXT:   (class reflect_Enum_MultiPayload_generic2.ClassTypeA)
// CHECK-NEXT:   (tuple))

// CHECK: Type info:
// X64-NEXT: (multi_payload_enum size=9 alignment=8 stride=16 num_extra_inhabitants=254 bitwise_takable=1
// X64-NEXT:   (case name=a index=0 offset=0
// X64-NEXT:     (reference kind=strong refcounting=native))
// X64-NEXT:   (case name=b index=1 offset=0
// X64-NEXT:     (tuple size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1)))

// CHECK: Mangled name: $s34reflect_Enum_MultiPayload_generic214SimplePayload1OyAA10ClassTypeACytG
// CHECK-NEXT: Demangled name: reflect_Enum_MultiPayload_generic2.SimplePayload1<reflect_Enum_MultiPayload_generic2.ClassTypeA, ()>

// CHECK: Enum value:
// CHECK-NEXT: (enum_value name=b index=1
// CHECK-NEXT: (tuple)
// CHECK-NEXT: )

doneReflecting()

// CHECK: Done.

