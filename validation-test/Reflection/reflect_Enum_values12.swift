// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_Enum_values12
// RUN: %target-codesign %t/reflect_Enum_values12

// RUN: %target-run %target-swift-reflection-test %t/reflect_Enum_values12 | tee /dev/stderr | %FileCheck %s --check-prefix=CHECK%target-ptrsize --check-prefix=CHECKALL --dump-input=fail %add_num_extra_inhabitants

// REQUIRES: objc_interop
// REQUIRES: executable_test
// REQUIRES: reflection_test_support
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan

import SwiftReflectionTest

indirect enum Outer<T> {
case a(T)
case b(T)
}

class C {
  var outer: Outer<Int>
  init() { outer = .a(99) }
}

reflect(enumValue: Outer<Int>.a(12))

// CHECKALL: Reflecting an enum value.
// CHECKALL-NEXT: Type reference:
// CHECKALL-NEXT: (bound_generic_enum reflect_Enum_values12.Outer
// CHECKALL-NEXT:   (struct Swift.Int))
// CHECKALL-NEXT: Value: .a(_)

reflect(enumValue: Outer<Int>.b(7))

// CHECKALL: Reflecting an enum value.
// CHECKALL-NEXT: Type reference:
// CHECKALL-NEXT: (bound_generic_enum reflect_Enum_values12.Outer
// CHECKALL-NEXT:   (struct Swift.Int))
// CHECKALL-NEXT: Value: .b(_)

reflect(enumValue: Optional<Outer<Int>>.some(.a(12)))

// CHECKALL: Reflecting an enum value.
// CHECKALL-NEXT: Type reference:
// CHECKALL-NEXT: (bound_generic_enum Swift.Optional
// CHECKALL-NEXT:   (bound_generic_enum reflect_Enum_values12.Outer
// CHECKALL-NEXT:     (struct Swift.Int)))
// CHECKALL-NEXT: Value: .some(.a(_))

reflect(enumValue: Optional<Outer<Int>>.some(.b(7)))

// CHECKALL: Reflecting an enum value.
// CHECKALL-NEXT: Type reference:
// CHECKALL-NEXT: (bound_generic_enum Swift.Optional
// CHECKALL-NEXT:   (bound_generic_enum reflect_Enum_values12.Outer
// CHECKALL-NEXT:     (struct Swift.Int)))
// CHECKALL-NEXT: Value: .some(.b(_))

reflect(enumValue: Optional<Outer<Int>>.none)

// CHECKALL: Reflecting an enum value.
// CHECKALL-NEXT: Type reference:
// CHECKALL-NEXT: (bound_generic_enum Swift.Optional
// CHECKALL-NEXT:   (bound_generic_enum reflect_Enum_values12.Outer
// CHECKALL-NEXT:     (struct Swift.Int)))
// CHECKALL-NEXT: Value: .none

reflect(object: C())

// CHECKALL: Reflecting an object.
// CHECKALL-NEXT: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECKALL-NEXT: Type reference:
// CHECKALL-NEXT: (class reflect_Enum_values12.C)

// CHECKALL: Type info:

// CHECK64-NEXT: (class_instance size=24 alignment=8 stride=24 num_extra_inhabitants=0 bitwise_takable=1
// CHECK64-NEXT:   (field name=outer offset=16
// x86_64 and arm64 have different spare bit pointer masks, hence different numbers of extra inhabitants here
// CHECK64-NEXT:     (multi_payload_enum size=8 alignment=8 stride=8 num_extra_inhabitants={{[0-9]+}} bitwise_takable=1

// CHECK32-NEXT: (class_instance size=12 alignment=4 stride=12 num_extra_inhabitants=0 bitwise_takable=1
// CHECK32-NEXT:   (field name=outer offset=8
// CHECK32-NEXT:     (multi_payload_enum size=4 alignment=4 stride=4 num_extra_inhabitants=2 bitwise_takable=1

// CHECKALL-NEXT:       (case name=a index=0 offset=0
// CHECKALL-NEXT:         (reference kind=strong refcounting=native))
// CHECKALL-NEXT:       (case name=b index=1 offset=0
// CHECKALL-NEXT:         (reference kind=strong refcounting=native)))))


doneReflecting()

// CHECKALL: Done.

