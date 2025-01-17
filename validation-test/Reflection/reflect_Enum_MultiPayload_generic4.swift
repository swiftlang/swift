// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_Enum_MultiPayload_generic4
// RUN: %target-codesign %t/reflect_Enum_MultiPayload_generic4

// RUN: %target-run %target-swift-reflection-test %t/reflect_Enum_MultiPayload_generic4 | tee /dev/stderr | %FileCheck %s --check-prefix=CHECK --check-prefix=X%target-ptrsize --dump-input=fail

// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan

import SwiftReflectionTest

struct StructWithEnumDepth0<T> {
  enum E {
  case t(T)
  case u(Int)
  }
  var e: E?
}

reflect(enum: StructWithEnumDepth0<Int>?.none)

// CHECK: Reflecting an enum.
// CHECK-NEXT: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (bound_generic_enum Swift.Optional
// CHECK-NEXT:   (bound_generic_struct reflect_Enum_MultiPayload_generic4.StructWithEnumDepth0
// CHECK-NEXT:     (struct Swift.Int)))

// MemoryLayout<> on ARM64 macOS gives 9,8,16 as the sizes of both SWED0<Int>? and SWED0<Int>.E
// from that, we can infer that SWED0<Int>.E must have a non-zero number of extra inhabitants

// CHECK: Type info:
// X64-NEXT: (single_payload_enum size=9 alignment=8 stride=16 num_extra_inhabitants=252 bitwise_takable=1
// X64-NEXT:   (case name=some index=0 offset=0
// X64-NEXT:     (struct size=9 alignment=8 stride=16 num_extra_inhabitants=253 bitwise_takable=1
// X64-NEXT:       (field name=e offset=0
// X64-NEXT:         (single_payload_enum size=9 alignment=8 stride=16 num_extra_inhabitants=253 bitwise_takable=1
// X64-NEXT:           (case name=some index=0 offset=0
// X64-NEXT:             (multi_payload_enum size=9 alignment=8 stride=16 num_extra_inhabitants=254 bitwise_takable=1
// X64-NEXT:               (case name=t index=0 offset=0
// X64-NEXT:                 (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// X64-NEXT:                   (field name=_value offset=0
// X64-NEXT:                     (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
// X64-NEXT:               (case name=u index=1 offset=0
// X64-NEXT:                 (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// X64-NEXT:                   (field name=_value offset=0
// X64-NEXT:                     (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))))
// X64-NEXT:           (case name=none index=1)))))
// X64-NEXT:   (case name=none index=1))

// CHECK: Mangled name: $s34reflect_Enum_MultiPayload_generic4010StructWithB6Depth0VySiGSg
// CHECK-NEXT: Demangled name: Swift.Optional<reflect_Enum_MultiPayload_generic4.StructWithEnumDepth0<Swift.Int>>

// CHECK: Enum value:
// CHECK-NEXT: (enum_value name=none index=1)

doneReflecting()

// CHECK: Done.

