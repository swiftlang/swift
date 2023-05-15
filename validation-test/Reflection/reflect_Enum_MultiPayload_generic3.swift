// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_Enum_MultiPayload_generic3
// RUN: %target-codesign %t/reflect_Enum_MultiPayload_generic3

// RUN: %target-run %target-swift-reflection-test %t/reflect_Enum_MultiPayload_generic3 | tee /dev/stderr | %FileCheck %s --check-prefix=CHECK --check-prefix=X%target-ptrsize --dump-input=fail

// REQUIRES: reflection_test_support
// REQUIRES: objc_interop
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib

import SwiftReflectionTest
import Darwin

private func debugLog(_ message: @autoclosure () -> String) {
  fputs("Child: \(message())\n", stderr)
  fflush(stderr)
}

struct StructWithEnumDepth0<T> {
  enum E {
  case t(T)
  case u(Void)
  }
  var e: E?
}

reflect(enum: StructWithEnumDepth0<Int>?.none)

// CHECK: Reflecting an enum.
// CHECK-NEXT: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (bound_generic_enum Swift.Optional
// CHECK-NEXT:   (bound_generic_struct reflect_Enum_MultiPayload_generic3.StructWithEnumDepth0
// CHECK-NEXT:     (struct Swift.Int)))

// MemoryLayout<> on ARM64 macOS gives
//   MemoryLayout<StructWithEnumDepth0<Int>.E>.size => 9
//   MemoryLayout<StructWithEnumDepth0<Int>>.size => 10
//   MemoryLayout<StructWithEnumDepth0<Int>?>.size => 11

// CHECK: Type info:
// X64-NEXT: (single_payload_enum size=11 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// X64-NEXT:   (case name=some index=0 offset=0
// X64-NEXT:     (struct size=10 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// X64-NEXT:       (field name=e offset=0
// X64-NEXT:         (single_payload_enum size=10 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// X64-NEXT:           (case name=some index=0 offset=0
// X64-NEXT:             (multi_payload_enum size=9 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
// X64-NEXT:               (case name=t index=0 offset=0
// X64-NEXT:                 (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// X64-NEXT:                   (field name=_value offset=0
// X64-NEXT:                     (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
// X64-NEXT:               (case name=u index=1 offset=0
// X64-NEXT:                 (tuple size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))))
// X64-NEXT:           (case name=none index=1)))))
// X64-NEXT:   (case name=none index=1))

// CHECK: Mangled name: $s34reflect_Enum_MultiPayload_generic3010StructWithB6Depth0VySiGSg
// CHECK-NEXT: Demangled name: Swift.Optional<reflect_Enum_MultiPayload_generic3.StructWithEnumDepth0<Swift.Int>>

// CHECK: Enum value:
// CHECK-NEXT: (enum_value name=none index=1)

doneReflecting()

// CHECK: Done.

