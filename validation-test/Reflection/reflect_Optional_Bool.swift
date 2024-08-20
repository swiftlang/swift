// RUN: %empty-directory(%t)
// RUN: %target-build-swift -g -lswiftSwiftReflectionTest %s -o %t/reflect_Optional_Bool
// RUN: %target-codesign %t/reflect_Optional_Bool

// RUN: %target-run %target-swift-reflection-test %t/reflect_Optional_Bool | %FileCheck %s 


// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan

import SwiftReflectionTest

let optFalse: Bool? = false
reflect(enum: optFalse)

// CHECK: Reflecting an enum.
// CHECK: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK: Type reference:
// CHECK: (bound_generic_enum Swift.Optional
// CHECK:   (struct Swift.Bool))

// CHECK: Type info:
// CHECK: (single_payload_enum size=1 alignment=1 stride=1 num_extra_inhabitants=253 bitwise_takable=1
// CHECK:   (case name=some index=0 offset=0
// CHECK:     (struct size=1 alignment=1 stride=1 num_extra_inhabitants=254 bitwise_takable=1
// CHECK:       (field name=_value offset=0
// CHECK:         (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=254 bitwise_takable=1))))
// CHECK:   (case name=none index=1))

// CHECK: Mangled name: $sSbSg
// CHECK: Demangled name: Swift.Optional<Swift.Bool>

// CHECK: Enum value:
// CHECK: (enum_value name=some index=0
// CHECK: (struct Swift.Bool)
// CHECK: )


let optTrue: Bool? = true
reflect(enum: optTrue)

// CHECK: Reflecting an enum.
// CHECK: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK: Type reference:
// CHECK: (bound_generic_enum Swift.Optional
// CHECK:   (struct Swift.Bool))

// CHECK: Type info:
// CHECK: (single_payload_enum size=1 alignment=1 stride=1 num_extra_inhabitants=253 bitwise_takable=1
// CHECK:   (case name=some index=0 offset=0
// CHECK:     (struct size=1 alignment=1 stride=1 num_extra_inhabitants=254 bitwise_takable=1
// CHECK:       (field name=_value offset=0
// CHECK:         (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=254 bitwise_takable=1))))
// CHECK:   (case name=none index=1))

// CHECK: Mangled name: $sSbSg
// CHECK: Demangled name: Swift.Optional<Swift.Bool>

// CHECK: Enum value:
// CHECK: (enum_value name=some index=0
// CHECK: (struct Swift.Bool)
// CHECK: )

let optNil: Bool? = .none
reflect(enum: optNil)

// CHECK: Reflecting an enum.
// CHECK: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK: Type reference:
// CHECK: (bound_generic_enum Swift.Optional
// CHECK:   (struct Swift.Bool))

// CHECK: Type info:
// CHECK: (single_payload_enum size=1 alignment=1 stride=1 num_extra_inhabitants=253 bitwise_takable=1
// CHECK:   (case name=some index=0 offset=0
// CHECK:     (struct size=1 alignment=1 stride=1 num_extra_inhabitants=254 bitwise_takable=1
// CHECK:       (field name=_value offset=0
// CHECK:         (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=254 bitwise_takable=1))))
// CHECK:   (case name=none index=1))

// CHECK: Mangled name: $sSbSg
// CHECK: Demangled name: Swift.Optional<Swift.Bool>

// CHECK: Enum value:
// CHECK: (enum_value name=none index=1) 

let optOptFalse: Bool?? = false
reflect(enum: optOptFalse)

// CHECK: Reflecting an enum.
// CHECK: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK: Type reference:
// CHECK: (bound_generic_enum Swift.Optional
// CHECK:   (bound_generic_enum Swift.Optional
// CHECK:     (struct Swift.Bool)))

// CHECK: Type info:
// CHECK: (single_payload_enum size=1 alignment=1 stride=1 num_extra_inhabitants=252 bitwise_takable=1
// CHECK:   (case name=some index=0 offset=0
// CHECK:     (single_payload_enum size=1 alignment=1 stride=1 num_extra_inhabitants=253 bitwise_takable=1
// CHECK:       (case name=some index=0 offset=0
// CHECK:         (struct size=1 alignment=1 stride=1 num_extra_inhabitants=254 bitwise_takable=1
// CHECK:           (field name=_value offset=0
// CHECK:             (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=254 bitwise_takable=1))))
// CHECK:       (case name=none index=1)))
// CHECK:   (case name=none index=1))

// CHECK: Mangled name: $sSbSgSg
// CHECK: Demangled name: Swift.Optional<Swift.Optional<Swift.Bool>>

// CHECK: Enum value:
// CHECK: (enum_value name=some index=0
// CHECK: (bound_generic_enum Swift.Optional
// CHECK:   (struct Swift.Bool))
// CHECK: )

let optOptTrue: Bool?? = true
reflect(enum: optOptTrue)

// CHECK: Reflecting an enum.
// CHECK: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK: Type reference:
// CHECK: (bound_generic_enum Swift.Optional
// CHECK:   (bound_generic_enum Swift.Optional
// CHECK:     (struct Swift.Bool)))

// CHECK: Type info:
// CHECK: (single_payload_enum size=1 alignment=1 stride=1 num_extra_inhabitants=252 bitwise_takable=1
// CHECK:   (case name=some index=0 offset=0
// CHECK:     (single_payload_enum size=1 alignment=1 stride=1 num_extra_inhabitants=253 bitwise_takable=1
// CHECK:       (case name=some index=0 offset=0
// CHECK:         (struct size=1 alignment=1 stride=1 num_extra_inhabitants=254 bitwise_takable=1
// CHECK:           (field name=_value offset=0
// CHECK:             (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=254 bitwise_takable=1))))
// CHECK:       (case name=none index=1)))
// CHECK:   (case name=none index=1))

// CHECK: Mangled name: $sSbSgSg
// CHECK: Demangled name: Swift.Optional<Swift.Optional<Swift.Bool>>

// CHECK: Enum value:
// CHECK: (enum_value name=some index=0
// CHECK: (bound_generic_enum Swift.Optional
// CHECK:   (struct Swift.Bool))
// CHECK: )

let optOptSomeNil: Bool?? = .some(.none)
reflect(enum: optOptSomeNil)

// CHECK: Reflecting an enum.
// CHECK: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK: Type reference:
// CHECK: (bound_generic_enum Swift.Optional
// CHECK:   (bound_generic_enum Swift.Optional
// CHECK:     (struct Swift.Bool)))

// CHECK: Type info:
// CHECK: (single_payload_enum size=1 alignment=1 stride=1 num_extra_inhabitants=252 bitwise_takable=1
// CHECK:   (case name=some index=0 offset=0
// CHECK:     (single_payload_enum size=1 alignment=1 stride=1 num_extra_inhabitants=253 bitwise_takable=1
// CHECK:       (case name=some index=0 offset=0
// CHECK:         (struct size=1 alignment=1 stride=1 num_extra_inhabitants=254 bitwise_takable=1
// CHECK:           (field name=_value offset=0
// CHECK:             (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=254 bitwise_takable=1))))
// CHECK:       (case name=none index=1)))
// CHECK:   (case name=none index=1))

// CHECK: Mangled name: $sSbSgSg
// CHECK: Demangled name: Swift.Optional<Swift.Optional<Swift.Bool>>

// CHECK: Enum value:
// CHECK: (enum_value name=some index=0
// CHECK: (bound_generic_enum Swift.Optional
// CHECK:   (struct Swift.Bool))
// CHECK: )

let optOptNil: Bool?? = .none
reflect(enum: optOptNil)

// CHECK: Reflecting an enum.
// CHECK: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK: Type reference:
// CHECK: (bound_generic_enum Swift.Optional
// CHECK:   (bound_generic_enum Swift.Optional
// CHECK:     (struct Swift.Bool)))

// CHECK: Type info:
// CHECK: (single_payload_enum size=1 alignment=1 stride=1 num_extra_inhabitants=252 bitwise_takable=1
// CHECK:   (case name=some index=0 offset=0
// CHECK:     (single_payload_enum size=1 alignment=1 stride=1 num_extra_inhabitants=253 bitwise_takable=1
// CHECK:       (case name=some index=0 offset=0
// CHECK:         (struct size=1 alignment=1 stride=1 num_extra_inhabitants=254 bitwise_takable=1
// CHECK:           (field name=_value offset=0
// CHECK:             (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=254 bitwise_takable=1))))
// CHECK:       (case name=none index=1)))
// CHECK:   (case name=none index=1))

// CHECK: Mangled name: $sSbSgSg
// CHECK: Demangled name: Swift.Optional<Swift.Optional<Swift.Bool>>

// CHECK: Enum value:
// CHECK: (enum_value name=none index=1)

doneReflecting()

// CHECK-64: Done.

