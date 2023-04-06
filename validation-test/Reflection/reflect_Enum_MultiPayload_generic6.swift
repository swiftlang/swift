// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_Enum_MultiPayload_generic6
// RUN: %target-codesign %t/reflect_Enum_MultiPayload_generic6

// RUN: %target-run %target-swift-reflection-test %t/reflect_Enum_MultiPayload_generic6 | tee /dev/stderr | %FileCheck %s --check-prefix=CHECK --check-prefix=X%target-ptrsize --dump-input=fail

// REQUIRES: reflection_test_support
// REQUIRES: objc_interop
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib

import SwiftReflectionTest
import Darwin

struct StructWithEnumDepth0<T> {
  enum E {
  case t(Void)
  case u(Void)
  case v(Never)
  case w(T)
  case x(Void)
  }
  var e: E?
}

reflect(enum: StructWithEnumDepth0<Int>?.none)

// CHECK: Reflecting an enum.
// CHECK-NEXT: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (bound_generic_enum Swift.Optional
// CHECK-NEXT:   (bound_generic_struct reflect_Enum_MultiPayload_generic6.StructWithEnumDepth0
// CHECK-NEXT:     (struct Swift.Int)))

// MemoryLayout<> on ARM64 macOS gives
//   MemoryLayout<StructWithEnumDepth0<Int>.E>.size => 9
//   MemoryLayout<StructWithEnumDepth0<Int>>.size => 10
//   MemoryLayout<StructWithEnumDepth0<Int>?>.size => 11

// Based on the above, E is not exporting any XIs, so it's
// getting handled as an SPE (MPEs export tag values as XIs).
// The TR for E is not a BoundGenericTypeRef
// The compiler is not providing a BuiltinTypeInfo

// CHECK: Type info:
// X64-NEXT: (single_payload_enum size=11 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// X64-NEXT:   (case name=some index=0 offset=0
// X64-NEXT:     (struct size=10 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// X64-NEXT:       (field name=e offset=0
// X64-NEXT:         (single_payload_enum size=10 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// X64-NEXT:           (case name=some index=0 offset=0
// X64-NEXT:             (multi_payload_enum size=9 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// X64-NEXT:               (case name=w index=0 offset=0
// X64-NEXT:                 (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// X64-NEXT:                   (field name=_value offset=0
// X64-NEXT:                     (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
// X64-NEXT:               (case name=t index=1 offset=0
// X64-NEXT:                 (tuple size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))
// X64-NEXT:               (case name=u index=2 offset=0
// X64-NEXT:                 (tuple size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))
// X64-NEXT:               (case name=v index=3 offset=0
// X64-NEXT:                 (no_payload_enum size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))
// X64-NEXT:               (case name=x index=4 offset=0
// X64-NEXT:                 (tuple size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))))
// X64-NEXT:           (case name=none index=1)))))
// X64-NEXT:   (case name=none index=1))

// CHECK: Mangled name: $s34reflect_Enum_MultiPayload_generic6010StructWithB6Depth0VySiGSg
// CHECK-NEXT: Demangled name: Swift.Optional<reflect_Enum_MultiPayload_generic6.StructWithEnumDepth0<Swift.Int>>

// CHECK: Enum value:
// CHECK-NEXT: (enum_value name=none index=1)

let example = StructWithEnumDepth0<Int>(e: StructWithEnumDepth0<Int>.E.w(17))
reflect(enum: example as StructWithEnumDepth0<Int>?)

// CHECK: Enum value:
// CHECK-NEXT: (enum_value name=some index=0
// CHECK-NEXT:   (bound_generic_struct reflect_Enum_MultiPayload_generic6.StructWithEnumDepth0
// CHECK-NEXT:    (struct Swift.Int))
// CHECK-NEXT: )

reflect(enum: example.e)

// CHECK: Enum value:
// CHECK-NEXT: (enum_value name=some index=0
// CHECK-NEXT: (bound_generic_enum reflect_Enum_MultiPayload_generic6.StructWithEnumDepth0.E
// CHECK-NEXT:   (bound_generic_struct reflect_Enum_MultiPayload_generic6.StructWithEnumDepth0
// CHECK-NEXT:     (struct Swift.Int)))
// CHECK-NEXT: )

reflect(enum: example.e!)

// CHECK: Enum value:
// CHECK-NEXT: (enum_value name=w index=0
// CHECK-NEXT: (struct Swift.Int)
// CHECK-NEXT: )

let example2 = StructWithEnumDepth0<Int>(e: StructWithEnumDepth0<Int>.E.t(()))
reflect(enum: example2 as StructWithEnumDepth0<Int>?)

// CHECK: Enum value:
// CHECK-NEXT: (enum_value name=some index=0
// CHECK-NEXT:   (bound_generic_struct reflect_Enum_MultiPayload_generic6.StructWithEnumDepth0
// CHECK-NEXT:    (struct Swift.Int))
// CHECK-NEXT: )

reflect(enum: example2.e!)

// CHECK: Enum value:
// CHECK-NEXT: (enum_value name=t index=1
// CHECK-NEXT: (tuple)
// CHECK-NEXT: )

let example3 = StructWithEnumDepth0<Int>(e: StructWithEnumDepth0<Int>.E.x(()))
reflect(enum: example3.e!)

// CHECK: Enum value:
// CHECK-NEXT: (enum_value name=x index=4
// CHECK-NEXT: (tuple)
// CHECK-NEXT: )


doneReflecting()

// CHECK: Done.

