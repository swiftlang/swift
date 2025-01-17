// validation-test/Reflection/reflect_Optional_Any.swift

// RUN: %empty-directory(%t)
// RUN: %target-build-swift -g -lswiftSwiftReflectionTest %s -o %t/reflect_Optional_Any
// RUN: %target-codesign %t/reflect_Optional_Any

// RUN: %target-run %target-swift-reflection-test %t/reflect_Optional_Any | %FileCheck %s --check-prefix=CHECK-%target-ptrsize %add_num_extra_inhabitants

// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan

import SwiftReflectionTest

struct TwentyFourByteStruct {
  let a: Int64
  let b: Int64
  let c: Int64
}

// ================================================================

let optionalAnyNonNil: Any? = TwentyFourByteStruct(a: 7, b: 8, c: 9)
reflect(enum: optionalAnyNonNil)

// CHECK-64: Reflecting an enum.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-64: Type reference:
// CHECK-64: (bound_generic_enum Swift.Optional
// CHECK-64:   (protocol_composition))

// CHECK-64: Type info:
// CHECK-64: (single_payload_enum size=32 alignment=8 stride=32 num_extra_inhabitants=[[#num_extra_inhabitants_64bit-1]] bitwise_takable=1
// CHECK-64:   (case name=some index=0 offset=0
// CHECK-64:     (opaque_existential size=32 alignment=8 stride=32 num_extra_inhabitants=[[#num_extra_inhabitants_64bit]] bitwise_takable=1
// CHECK-64:       (field name=metadata offset=24
// CHECK-64:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=[[#num_extra_inhabitants_64bit]] bitwise_takable=1))))
// CHECK-64:   (case name=none index=1))

// CHECK-64: Mangled name: $sypSg
// CHECK-64: Demangled name: Swift.Optional<Any>

// CHECK-64: Enum value:
// CHECK-64: (enum_value name=some index=0
// CHECK-64:  (protocol_composition)
// CHECK-64: )

// CHECK-32: Reflecting an enum.
// CHECK-32: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-32: Type reference:
// CHECK-32: (bound_generic_enum Swift.Optional
// CHECK-32:  (protocol_composition))

// CHECK-32: Type info:
// CHECK-32: (single_payload_enum size=16 alignment=4 stride=16 num_extra_inhabitants=4095 bitwise_takable=1
// CHECK-32:   (case name=some index=0 offset=0
// CHECK-32:     (opaque_existential size=16 alignment=4 stride=16 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32:       (field name=metadata offset=12
// CHECK-32:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1))))
// CHECK-32:   (case name=none index=1))

// CHECK-32: Mangled name: $sypSg
// CHECK-32: Demangled name: Swift.Optional<Any>

// CHECK-32: Enum value:
// CHECK-32: (enum_value name=some index=0
// CHECK-32:  (protocol_composition)
// CHECK-32: )

// ================================================================

let optionalAnyNil: Any? = nil
reflect(enum: optionalAnyNil)

// CHECK-64: Reflecting an enum.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-64: Type reference:
// CHECK-64: (bound_generic_enum Swift.Optional
// CHECK-64:   (protocol_composition))

// CHECK-64: Type info:
// CHECK-64: (single_payload_enum size=32 alignment=8 stride=32 num_extra_inhabitants=[[#num_extra_inhabitants_64bit-1]] bitwise_takable=1
// CHECK-64:   (case name=some index=0 offset=0
// CHECK-64:     (opaque_existential size=32 alignment=8 stride=32 num_extra_inhabitants=[[#num_extra_inhabitants_64bit]] bitwise_takable=1
// CHECK-64:       (field name=metadata offset=24
// CHECK-64:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=[[#num_extra_inhabitants_64bit]] bitwise_takable=1))))
// CHECK-64:   (case name=none index=1))

// CHECK-64: Mangled name: $sypSg
// CHECK-64: Demangled name: Swift.Optional<Any>

// CHECK-64: Enum value:
// CHECK-64: (enum_value name=none index=1)


// CHECK-32: Reflecting an enum.
// CHECK-32: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-32: Type reference:
// CHECK-32: (bound_generic_enum Swift.Optional
// CHECK-32:   (protocol_composition))

// CHECK-32: Type info:
// CHECK-32: (single_payload_enum size=16 alignment=4 stride=16 num_extra_inhabitants=4095 bitwise_takable=1
// CHECK-32:   (case name=some index=0 offset=0
// CHECK-32:     (opaque_existential size=16 alignment=4 stride=16 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32:       (field name=metadata offset=12
// CHECK-32:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1))))
// CHECK-32:   (case name=none index=1))

// CHECK-32: Mangled name: $sypSg
// CHECK-32: Demangled name: Swift.Optional<Any>

// CHECK-32: Enum value:
// CHECK-32: (enum_value name=none index=1)

// ================================================================

let optionalOptionalAnyNil: Any?? = nil
reflect(enum: optionalOptionalAnyNil)

// CHECK-64: Reflecting an enum.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-64: Type reference:
// CHECK-64: (bound_generic_enum Swift.Optional
// CHECK-64:   (bound_generic_enum Swift.Optional
// CHECK-64:     (protocol_composition)))

// CHECK-64: Type info:
// CHECK-64: (single_payload_enum size=32 alignment=8 stride=32 num_extra_inhabitants=[[#num_extra_inhabitants_64bit-2]] bitwise_takable=1
// CHECK-64:   (case name=some index=0 offset=0
// CHECK-64:     (single_payload_enum size=32 alignment=8 stride=32 num_extra_inhabitants=[[#num_extra_inhabitants_64bit-1]] bitwise_takable=1
// CHECK-64:       (case name=some index=0 offset=0
// CHECK-64:         (opaque_existential size=32 alignment=8 stride=32 num_extra_inhabitants=[[#num_extra_inhabitants_64bit]] bitwise_takable=1
// CHECK-64:           (field name=metadata offset=24
// CHECK-64:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=[[#num_extra_inhabitants_64bit]] bitwise_takable=1))))
// CHECK-64:       (case name=none index=1)))
// CHECK-64:   (case name=none index=1))

// CHECK-64: Mangled name: $sypSgSg
// CHECK-64: Demangled name: Swift.Optional<Swift.Optional<Any>>

// CHECK-64: Enum value:
// CHECK-64: (enum_value name=none index=1)

// CHECK-32: Reflecting an enum.
// CHECK-32: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-32: Type reference:
// CHECK-32: (bound_generic_enum Swift.Optional
// CHECK-32:   (bound_generic_enum Swift.Optional
// CHECK-32:     (protocol_composition)))

// CHECK-32: Type info:
// CHECK-32: (single_payload_enum size=16 alignment=4 stride=16 num_extra_inhabitants=4094 bitwise_takable=1
// CHECK-32:   (case name=some index=0 offset=0
// CHECK-32:     (single_payload_enum size=16 alignment=4 stride=16 num_extra_inhabitants=4095 bitwise_takable=1
// CHECK-32:       (case name=some index=0 offset=0
// CHECK-32:         (opaque_existential size=16 alignment=4 stride=16 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32:           (field name=metadata offset=12
// CHECK-32:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1))))
// CHECK-32:       (case name=none index=1)))
// CHECK-32:   (case name=none index=1))

// CHECK-32: Mangled name: $sypSgSg
// CHECK-32: Demangled name: Swift.Optional<Swift.Optional<Any>>

// CHECK-32: Enum value:
// CHECK-32: (enum_value name=none index=1)

// ================================================================

let optionalOptionalAnySomeNil: Any?? = .some(nil)
reflect(enum: optionalOptionalAnySomeNil)

// CHECK-64: Reflecting an enum.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-64: Type reference:
// CHECK-64: (bound_generic_enum Swift.Optional
// CHECK-64:   (bound_generic_enum Swift.Optional
// CHECK-64:     (protocol_composition)))

// CHECK-64: Type info:
// CHECK-64: (single_payload_enum size=32 alignment=8 stride=32 num_extra_inhabitants=[[#num_extra_inhabitants_64bit-2]] bitwise_takable=1
// CHECK-64:   (case name=some index=0 offset=0
// CHECK-64:     (single_payload_enum size=32 alignment=8 stride=32 num_extra_inhabitants=[[#num_extra_inhabitants_64bit-1]] bitwise_takable=1
// CHECK-64:       (case name=some index=0 offset=0
// CHECK-64:         (opaque_existential size=32 alignment=8 stride=32 num_extra_inhabitants=[[#num_extra_inhabitants_64bit]] bitwise_takable=1
// CHECK-64:           (field name=metadata offset=24
// CHECK-64:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=[[#num_extra_inhabitants_64bit]] bitwise_takable=1))))
// CHECK-64:       (case name=none index=1)))
// CHECK-64:   (case name=none index=1))

// CHECK-64: Mangled name: $sypSgSg
// CHECK-64: Demangled name: Swift.Optional<Swift.Optional<Any>>

// CHECK-64: Enum value:
// CHECK-64: (enum_value name=some index=0
// CHECK-64: (bound_generic_enum Swift.Optional
// CHECK-64:   (protocol_composition))
// CHECK-64: )

// CHECK-32: Reflecting an enum.
// CHECK-32: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-32: Type reference:
// CHECK-32: (bound_generic_enum Swift.Optional
// CHECK-32:   (bound_generic_enum Swift.Optional
// CHECK-32:     (protocol_composition)))

// CHECK-32: Type info:
// CHECK-32: (single_payload_enum size=16 alignment=4 stride=16 num_extra_inhabitants=4094 bitwise_takable=1
// CHECK-32:   (case name=some index=0 offset=0
// CHECK-32:     (single_payload_enum size=16 alignment=4 stride=16 num_extra_inhabitants=4095 bitwise_takable=1
// CHECK-32:       (case name=some index=0 offset=0
// CHECK-32:         (opaque_existential size=16 alignment=4 stride=16 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32:           (field name=metadata offset=12
// CHECK-32:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1))))
// CHECK-32:       (case name=none index=1)))
// CHECK-32:   (case name=none index=1))

// CHECK-32: Mangled name: $sypSgSg
// CHECK-32: Demangled name: Swift.Optional<Swift.Optional<Any>>

// CHECK-32: Enum value:
// CHECK-32: (enum_value name=some index=0
// CHECK-32: (bound_generic_enum Swift.Optional
// CHECK-32:   (protocol_composition))
// CHECK-32: )

// ================================================================

let optionalOptionalAnyNonNil: Any?? = .some(.some(7))
reflect(enum: optionalOptionalAnyNonNil)

// CHECK-64: Reflecting an enum.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-64: Type reference:
// CHECK-64: (bound_generic_enum Swift.Optional
// CHECK-64:   (bound_generic_enum Swift.Optional
// CHECK-64:     (protocol_composition)))

// CHECK-64: Type info:
// CHECK-64: (single_payload_enum size=32 alignment=8 stride=32 num_extra_inhabitants=[[#num_extra_inhabitants_64bit-2]] bitwise_takable=1
// CHECK-64:   (case name=some index=0 offset=0
// CHECK-64:     (single_payload_enum size=32 alignment=8 stride=32 num_extra_inhabitants=[[#num_extra_inhabitants_64bit-1]] bitwise_takable=1
// CHECK-64:       (case name=some index=0 offset=0
// CHECK-64:         (opaque_existential size=32 alignment=8 stride=32 num_extra_inhabitants=[[#num_extra_inhabitants_64bit]] bitwise_takable=1
// CHECK-64:           (field name=metadata offset=24
// CHECK-64:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=[[#num_extra_inhabitants_64bit]] bitwise_takable=1))))
// CHECK-64:       (case name=none index=1)))
// CHECK-64:   (case name=none index=1))

// CHECK-64: Mangled name: $sypSgSg
// CHECK-64: Demangled name: Swift.Optional<Swift.Optional<Any>>

// CHECK-64: Enum value:
// CHECK-64: (enum_value name=some index=0
// CHECK-64: (bound_generic_enum Swift.Optional
// CHECK-64:   (protocol_composition))
// CHECK-64: )

// CHECK-32: Reflecting an enum.
// CHECK-32: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-32: Type reference:
// CHECK-32: (bound_generic_enum Swift.Optional
// CHECK-32:   (bound_generic_enum Swift.Optional
// CHECK-32:     (protocol_composition)))

// CHECK-32: Type info:
// CHECK-32: (single_payload_enum size=16 alignment=4 stride=16 num_extra_inhabitants=4094 bitwise_takable=1
// CHECK-32:   (case name=some index=0 offset=0
// CHECK-32:     (single_payload_enum size=16 alignment=4 stride=16 num_extra_inhabitants=4095 bitwise_takable=1
// CHECK-32:       (case name=some index=0 offset=0
// CHECK-32:         (opaque_existential size=16 alignment=4 stride=16 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32:           (field name=metadata offset=12
// CHECK-32:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1))))
// CHECK-32:       (case name=none index=1)))
// CHECK-32:   (case name=none index=1))

// CHECK-32: Mangled name: $sypSgSg
// CHECK-32: Demangled name: Swift.Optional<Swift.Optional<Any>>

// CHECK-32: Enum value:
// CHECK-32: (enum_value name=some index=0
// CHECK-32: (bound_generic_enum Swift.Optional
// CHECK-32:   (protocol_composition))
// CHECK-32: )

// ================================================================

doneReflecting()

// CHECK-64: Done.

// CHECK-32: Done.
