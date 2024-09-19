// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_Enum_CF
// RUN: %target-codesign %t/reflect_Enum_CF

// RUN: %target-run %target-swift-reflection-test %t/reflect_Enum_CF | tee /dev/stderr | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK%target-ptrsize --dump-input=fail %add_num_extra_inhabitants

// REQUIRES: objc_interop
// REQUIRES: executable_test
// REQUIRES: reflection_test_support
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan

import SwiftReflectionTest
import Foundation

enum MyPair<T,U> {
case a(T)
case b(U)
}

reflect(enum: MyPair<Void,CFNumber>.b(kCFNumberPositiveInfinity))

// CHECK: Reflecting an enum.
// CHECK-NEXT: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (bound_generic_enum reflect_Enum_CF.MyPair
// CHECK-NEXT:   (tuple)
// CHECK-NEXT:   (foreign name=So11CFNumberRefa))

// CHECK: Type info:
// CHECK64-NEXT: (multi_payload_enum size=9 alignment=8 stride=16 num_extra_inhabitants=254 bitwise_takable=1
// CHECK64-NEXT:   (case name=a index=0 offset=0
// CHECK64-NEXT:     (tuple size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))
// CHECK64-NEXT:   (case name=b index=1 offset=0
// CHECK64-NEXT:     (reference kind=strong refcounting=unknown)))

// CHECK: Mangled name: $s15reflect_Enum_CF6MyPairOyytSo11CFNumberRefaG
// CHECK-NEXT: Demangled name: reflect_Enum_CF.MyPair<(), __C.CFNumberRef>

// CHECK: Enum value:
// CHECK-NEXT: (enum_value name=b index=1
// CHECK-NEXT:   (foreign name=So11CFNumberRefa)
// CHECK-NEXT: )

struct StructA {
  let field1 = MyPair<Void,CFNumber>.b(kCFNumberPositiveInfinity)
  let field2 = 7
}

enum T {
case a
case b(MyPair<Void,CFNumber>)
}

reflect(enum: StructA().field1)

// CHECK: Reflecting an enum.
// CHECK-NEXT: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (bound_generic_enum reflect_Enum_CF.MyPair
// CHECK-NEXT:   (tuple)
// CHECK-NEXT:   (foreign name=So11CFNumberRefa))

// CHECK: Type info:
// CHECK64-NEXT: (multi_payload_enum size=9 alignment=8 stride=16 num_extra_inhabitants=254 bitwise_takable=1
// CHECK64-NEXT:   (case name=a index=0 offset=0
// CHECK64-NEXT:     (tuple size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))
// CHECK64-NEXT:   (case name=b index=1 offset=0
// CHECK64-NEXT:     (reference kind=strong refcounting=unknown)))

// CHECK: Mangled name: $s15reflect_Enum_CF6MyPairOyytSo11CFNumberRefaG
// CHECK-NEXT: Demangled name: reflect_Enum_CF.MyPair<(), __C.CFNumberRef>

// CHECK: Enum value:
// CHECK-NEXT: (enum_value name=b index=1
// CHECK-NEXT: (foreign name=So11CFNumberRefa)
// CHECK-NEXT: )

reflect(enum: T.a)

// CHECK: Reflecting an enum.
// CHECK-NEXT: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (enum reflect_Enum_CF.T)

// CHECK: Type info:
// CHECK64-NEXT: (single_payload_enum size=9 alignment=8 stride=16 num_extra_inhabitants=253 bitwise_takable=1
// CHECK64-NEXT:   (case name=b index=0 offset=0
// CHECK64-NEXT:     (multi_payload_enum size=9 alignment=8 stride=16 num_extra_inhabitants=254 bitwise_takable=1
// CHECK64-NEXT:       (case name=a index=0 offset=0
// CHECK64-NEXT:         (tuple size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))
// CHECK64-NEXT:       (case name=b index=1 offset=0
// CHECK64-NEXT:         (reference kind=strong refcounting=unknown))))
// CHECK64-NEXT:   (case name=a index=1))

// CHECK: Mangled name: $s15reflect_Enum_CF1TO
// CHECK-NEXT: Demangled name: reflect_Enum_CF.T

// CHECK: Enum value:
// CHECK-NEXT: (enum_value name=a index=1)

doneReflecting()

// CHECKALL: Done.

