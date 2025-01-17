// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_Enum_MultiPayload_generic_empty
// RUN: %target-codesign %t/reflect_Enum_MultiPayload_generic_empty

// RUN: %target-run %target-swift-reflection-test %t/reflect_Enum_MultiPayload_generic_empty | tee /dev/stderr | %FileCheck %s --check-prefix=CHECK --check-prefix=X%target-ptrsize --dump-input=fail

// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan

import SwiftReflectionTest

struct S {
}

public enum AppearanceBasedConfiguration<T> {
  case single(T)
  case appearanceBased(light: T, dark: T)
}

reflect(enum: AppearanceBasedConfiguration<S>.single(S()))

// CHECK: Reflecting an enum.
// CHECK-NEXT: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (bound_generic_enum reflect_Enum_MultiPayload_generic_empty.AppearanceBasedConfiguration
// CHECK-NEXT:   (struct reflect_Enum_MultiPayload_generic_empty.S))

// CHECK: Type info:
// X64-NEXT: (multi_payload_enum size=1 alignment=1 stride=1 num_extra_inhabitants=254 bitwise_takable=1
// X64-NEXT:   (case name=single index=0 offset=0
// X64-NEXT:     (struct size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))
// X64-NEXT:   (case name=appearanceBased index=1 offset=0
// X64-NEXT:     (tuple size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// X64-NEXT:       (field offset=0
// X64-NEXT:         (struct size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))
// X64-NEXT:       (field offset=0
// X64-NEXT:         (struct size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1)))))

// CHECK: Mangled name: $s39reflect_Enum_MultiPayload_generic_empty28AppearanceBasedConfigurationOyAA1SVG
// CHECK-NEXT: Demangled name: reflect_Enum_MultiPayload_generic_empty.AppearanceBasedConfiguration<reflect_Enum_MultiPayload_generic_empty.S>

// CHECK: Enum value:
// CHECK-NEXT: (enum_value name=single index=0
// CHECK-NEXT: (struct reflect_Enum_MultiPayload_generic_empty.S)
// CHECK-NEXT: )

reflect(enum: AppearanceBasedConfiguration<S>.appearanceBased(light: S(), dark: S()))

// CHECK: Reflecting an enum.
// CHECK-NEXT: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (bound_generic_enum reflect_Enum_MultiPayload_generic_empty.AppearanceBasedConfiguration
// CHECK-NEXT:   (struct reflect_Enum_MultiPayload_generic_empty.S))

// CHECK: Type info:
// X64-NEXT: (multi_payload_enum size=1 alignment=1 stride=1 num_extra_inhabitants=254 bitwise_takable=1
// X64-NEXT:   (case name=single index=0 offset=0
// X64-NEXT:     (struct size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))
// X64-NEXT:   (case name=appearanceBased index=1 offset=0
// X64-NEXT:     (tuple size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// X64-NEXT:       (field offset=0
// X64-NEXT:         (struct size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))
// X64-NEXT:       (field offset=0
// X64-NEXT:         (struct size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1)))))

// CHECK: Mangled name: $s39reflect_Enum_MultiPayload_generic_empty28AppearanceBasedConfigurationOyAA1SVG
// CHECK-NEXT: Demangled name: reflect_Enum_MultiPayload_generic_empty.AppearanceBasedConfiguration<reflect_Enum_MultiPayload_generic_empty.S>

// CHECK: Enum value:
// CHECK-NEXT: (enum_value name=appearanceBased index=1
// CHECK-NEXT: (tuplelight = 
// CHECK-NEXT:   (struct reflect_Enum_MultiPayload_generic_empty.S)dark = 
// CHECK-NEXT:   (struct reflect_Enum_MultiPayload_generic_empty.S))
// CHECK-NEXT: )

doneReflecting()

// CHECK: Done.
