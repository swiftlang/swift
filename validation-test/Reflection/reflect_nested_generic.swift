// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_nested_generic
// RUN: %target-codesign %t/reflect_nested_generic

// RUN: %target-run %target-swift-reflection-test %t/reflect_nested_generic | tee /dev/stderr | %FileCheck %s --check-prefix=CHECK --check-prefix=X%target-ptrsize --dump-input=fail

// REQUIRES: reflection_test_support
// REQUIRES: objc_interop
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib

import SwiftReflectionTest

enum Outer1 {
  class C<T> {
    enum Inner {
      struct S<U> {
	var u: U? = nil
	var t: T? = nil
      }
    }
  }
}

reflect(enum: Outer1.C<Void>.Inner.S<(Int,Int)>?.none)

//CHECK: Reflecting an enum.
//CHECK-NEXT: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
//CHECK-NEXT: Type reference:
//CHECK-NEXT: (bound_generic_enum Swift.Optional
//CHECK-NEXT:   (bound_generic_struct reflect_nested_generic.Outer1.C.Inner.S
//CHECK-NEXT:     (tuple
//CHECK-NEXT:       (struct Swift.Int)
//CHECK-NEXT:       (struct Swift.Int))
//CHECK-NEXT:     (bound_generic_class reflect_nested_generic.Outer1.C
//CHECK-NEXT:       (tuple))))

//CHECK: Type info:

// This is the layout for 64-bit targets (Note, this example
// has no pointer-based elements, so we don't need to distinguish
// extra inhabitants for platforms with varying pointer layouts.)

//X64-NEXT: (single_payload_enum size=19 alignment=8 stride=24 num_extra_inhabitants=0 bitwise_takable=1
//X64-NEXT:   (case name=some index=0 offset=0
//X64-NEXT:     (struct size=18 alignment=8 stride=24 num_extra_inhabitants=0 bitwise_takable=1
//X64-NEXT:       (field name=u offset=0
//X64-NEXT:         (single_payload_enum size=17 alignment=8 stride=24 num_extra_inhabitants=0 bitwise_takable=1
//X64-NEXT:           (case name=some index=0 offset=0
//X64-NEXT:             (tuple size=16 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
//X64-NEXT:               (field offset=0
//X64-NEXT:                 (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
//X64-NEXT:                   (field name=_value offset=0
//X64-NEXT:                     (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
//X64-NEXT:               (field offset=8
//X64-NEXT:                 (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
//X64-NEXT:                   (field name=_value offset=0
//X64-NEXT:                     (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))))
//X64-NEXT:           (case name=none index=1)))
//X64-NEXT:       (field name=t offset=17
//X64-NEXT:         (single_payload_enum size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
//X64-NEXT:           (case name=some index=0 offset=0
//X64-NEXT:             (tuple size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))
//X64-NEXT:           (case name=none index=1)))))
//X64-NEXT:   (case name=none index=1))

//TODO:  Work out the layout for 32-bit targets

//CHECK: Mangled name: $s22reflect_nested_generic6Outer1O1CC1SVy_yt_Si_SitGSg
//CHECK-NEXT: Demangled name: Swift.Optional<reflect_nested_generic.Outer1.C<()>.S<(Swift.Int, Swift.Int)>>

//CHECK: Enum value:
//CHECK-NEXT: (enum_value name=none index=1)


protocol P {}

struct S1: P { var a: Int; var b: Int }
struct S2: P { }

struct Outer2 {
  enum E<T: P> {
    struct Inner {
      enum F<U: P> {
        struct Innerer {
	  var u: U? = nil
	}
      case u(U)
      case a
      case b
      }
    }
  }
}

reflect(enum: Outer2.E<S1>.Inner.F<S2>.u(S2()))

//CHECK: Reflecting an enum.
//CHECK-NEXT: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
//CHECK-NEXT: Type reference:
//CHECK-NEXT: (bound_generic_enum reflect_nested_generic.Outer2.E.Inner.F
//CHECK-NEXT:   (struct reflect_nested_generic.S2)
//CHECK-NEXT:   (bound_generic_enum reflect_nested_generic.Outer2.E
//CHECK-NEXT:     (struct reflect_nested_generic.S1)))

// Note: layout here is same for both 32- and 64-bit platforms

//CHECK: Type info:
//CHECK-NEXT: (single_payload_enum size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
//CHECK-NEXT:   (case name=u index=0 offset=0
//CHECK-NEXT:     (struct size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))
//CHECK-NEXT:   (case name=a index=1)
//CHECK-NEXT:   (case name=b index=2))

//CHECK: Mangled name: $s22reflect_nested_generic6Outer2V1EO1FOy_AA2S1V_AA2S2VG
//CHECK-NEXT: Demangled name: reflect_nested_generic.Outer2.E<reflect_nested_generic.S1>.F<reflect_nested_generic.S2>

//CHECK: Enum value:
//CHECK-NEXT: (enum_value name=u index=0
//CHECK-NEXT:   (struct reflect_nested_generic.S2)
//CHECK-NEXT: )

reflect(enum: Outer2.E<S1>.Inner.F<S2>.b)

//CHECK: Reflecting an enum.
//CHECK: Type info:
//CHECK: Mangled name: $s22reflect_nested_generic6Outer2V1EO1FOy_AA2S1V_AA2S2VG

//CHECK: Enum value:
//CHECK-NEXT: (enum_value name=b index=2)

reflect(enum: Outer2.E<S1>.Inner.F<S2>.b as Outer2.E<S1>.Inner.F<S2>??)

//CHECK: Reflecting an enum.
//CHECK: Type info:
//CHECK-NEXT: (single_payload_enum size=3 alignment=1 stride=3 num_extra_inhabitants=0 bitwise_takable=1
//CHECK-NEXT:   (case name=some index=0 offset=0
//CHECK-NEXT:     (single_payload_enum size=2 alignment=1 stride=2 num_extra_inhabitants=0 bitwise_takable=1
//CHECK-NEXT:       (case name=some index=0 offset=0
//CHECK-NEXT:         (single_payload_enum size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
//CHECK-NEXT:           (case name=u index=0 offset=0
//CHECK-NEXT:             (struct size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))
//CHECK-NEXT:           (case name=a index=1)
//CHECK-NEXT:           (case name=b index=2)))
//CHECK-NEXT:       (case name=none index=1)))
//CHECK-NEXT:   (case name=none index=1))

//CHECK: Mangled name: $s22reflect_nested_generic6Outer2V1EO1FOy_AA2S1V_AA2S2VGSgSg
//CHECK-NEXT: Demangled name: Swift.Optional<Swift.Optional<reflect_nested_generic.Outer2.E<reflect_nested_generic.S1>.F<reflect_nested_generic.S2>>>

//CHECK: Enum value:
//CHECK-NEXT: (enum_value name=some index=0
//CHECK-NEXT: (bound_generic_enum Swift.Optional
//CHECK-NEXT:   (bound_generic_enum reflect_nested_generic.Outer2.E.Inner.F
//CHECK-NEXT:     (struct reflect_nested_generic.S2)
//CHECK-NEXT:     (bound_generic_enum reflect_nested_generic.Outer2.E
//CHECK-NEXT:       (struct reflect_nested_generic.S1))))
//CHECK-NEXT: )

reflect(enum: Outer2.E<S1>.Inner.F<S2>??.none)

//CHECK: Reflecting an enum.
//CHECK: Type info:
//CHECK: Mangled name: $s22reflect_nested_generic6Outer2V1EO1FOy_AA2S1V_AA2S2VGSgSg
//CHECK: Demangled name: Swift.Optional<Swift.Optional<reflect_nested_generic.Outer2.E<reflect_nested_generic.S1>.F<reflect_nested_generic.S2>>>

//CHECK: Enum value:
//CHECK-NEXT: (enum_value name=none index=1)

struct S3: P { var a: UInt8 = 0 }

reflect(enum: Outer2.E<S2>.Inner.F<S3>.u(S3()))

//CHECK: Reflecting an enum.
//CHECK-NEXT: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}

//CHECK: Type reference:
//CHECK-NEXT: (bound_generic_enum reflect_nested_generic.Outer2.E.Inner.F
//CHECK-NEXT:   (struct reflect_nested_generic.S3)
//CHECK-NEXT:   (bound_generic_enum reflect_nested_generic.Outer2.E
//CHECK-NEXT:     (struct reflect_nested_generic.S2)))

//CHECK: Type info:
//CHECK-NEXT: (single_payload_enum size=2 alignment=1 stride=2 num_extra_inhabitants=0 bitwise_takable=1
//CHECK-NEXT:   (case name=u index=0 offset=0
//CHECK-NEXT:     (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
//CHECK-NEXT:       (field name=a offset=0
//CHECK-NEXT:         (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
//CHECK-NEXT:           (field name=_value offset=0
//CHECK-NEXT:             (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))))))
//CHECK-NEXT:   (case name=a index=1)
//CHECK-NEXT:   (case name=b index=2))

//CHECK: Mangled name: $s22reflect_nested_generic6Outer2V1EO1FOy_AA2S2V_AA2S3VG
//CHECK-NEXT: Demangled name: reflect_nested_generic.Outer2.E<reflect_nested_generic.S2>.F<reflect_nested_generic.S3>

//CHECK: Enum value:
//CHECK-NEXT: (enum_value name=u index=0
//CHECK-NEXT: (struct reflect_nested_generic.S3)
//CHECK-NEXT: )


reflect(enum: Outer2.E<S2>.Inner.F<S3>.b)

//CHECK: Reflecting an enum.
//CHECK: Type reference:
//CHECK: Type info:
//CHECK: Mangled name: $s22reflect_nested_generic6Outer2V1EO1FOy_AA2S2V_AA2S3VG

//CHECK: Enum value:
//CHECK-NEXT: (enum_value name=b index=2)

reflect(enum: Outer2.E<S2>.Inner.F<S3>??.none)

//CHECK: Reflecting an enum.
//CHECK: Type reference:
//CHECK: Type info:
//CHECK-NEXT: (single_payload_enum size=4 alignment=1 stride=4 num_extra_inhabitants=0 bitwise_takable=1
//CHECK-NEXT:   (case name=some index=0 offset=0
//CHECK-NEXT:     (single_payload_enum size=3 alignment=1 stride=3 num_extra_inhabitants=0 bitwise_takable=1
//CHECK-NEXT:       (case name=some index=0 offset=0
//CHECK-NEXT:         (single_payload_enum size=2 alignment=1 stride=2 num_extra_inhabitants=0 bitwise_takable=1
//CHECK-NEXT:           (case name=u index=0 offset=0
//CHECK-NEXT:             (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
//CHECK-NEXT:               (field name=a offset=0
//CHECK-NEXT:                 (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
//CHECK-NEXT:                   (field name=_value offset=0
//CHECK-NEXT:                     (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))))))
//CHECK-NEXT:           (case name=a index=1)
//CHECK-NEXT:           (case name=b index=2)))
//CHECK-NEXT:       (case name=none index=1)))
//CHECK-NEXT:   (case name=none index=1))

//CHECK: Mangled name: $s22reflect_nested_generic6Outer2V1EO1FOy_AA2S2V_AA2S3VGSgSg
//CHECK: Demangled name: Swift.Optional<Swift.Optional<reflect_nested_generic.Outer2.E<reflect_nested_generic.S2>.F<reflect_nested_generic.S3>>>

//CHECK: Enum value:
//CHECK-NEXT: (enum_value name=none index=1)

struct S4: P { var a: Bool = true }

reflect(enum: Outer2.E<S3>.Inner.F<S4>.b)

//CHECK: Reflecting an enum.
//CHECK: Type reference:
//CHECK: Type info:
//CHECK-NEXT: (single_payload_enum size=1 alignment=1 stride=1 num_extra_inhabitants=252 bitwise_takable=1
//CHECK-NEXT:   (case name=u index=0 offset=0
//CHECK-NEXT:     (struct size=1 alignment=1 stride=1 num_extra_inhabitants=254 bitwise_takable=1
//CHECK-NEXT:       (field name=a offset=0
//CHECK-NEXT:         (struct size=1 alignment=1 stride=1 num_extra_inhabitants=254 bitwise_takable=1
//CHECK-NEXT:           (field name=_value offset=0
//CHECK-NEXT:             (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=254 bitwise_takable=1))))))
//CHECK-NEXT:   (case name=a index=1)
//CHECK-NEXT:   (case name=b index=2))

//CHECK: Mangled name: $s22reflect_nested_generic6Outer2V1EO1FOy_AA2S3V_AA2S4VG
//CHECK: Demangled name: reflect_nested_generic.Outer2.E<reflect_nested_generic.S3>.F<reflect_nested_generic.S4>

//CHECK: Enum value:
//CHECK-NEXT: (enum_value name=b index=2)

reflect(enum: Outer2.E<S1>.Inner.F<S2>.Innerer?.none)

//CHECK: Reflecting an enum.
//CHECK-NEXT: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
//CHECK-NEXT: Type reference:
//CHECK-NEXT: (bound_generic_enum Swift.Optional
//CHECK-NEXT:   (bound_generic_struct reflect_nested_generic.Outer2.E.Inner.F.Innerer
//CHECK-NEXT:     (bound_generic_enum reflect_nested_generic.Outer2.E.Inner.F
//CHECK-NEXT:       (struct reflect_nested_generic.S2)
//CHECK-NEXT:       (bound_generic_enum reflect_nested_generic.Outer2.E
//CHECK-NEXT:         (struct reflect_nested_generic.S1)))))

// This layout is the same for 32-bit or 64-bit platforms

//CHECK: Type info:
//CHECK-NEXT: (single_payload_enum size=2 alignment=1 stride=2 num_extra_inhabitants=0 bitwise_takable=1
//CHECK-NEXT:   (case name=some index=0 offset=0
//CHECK-NEXT:     (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
//CHECK-NEXT:       (field name=u offset=0
//CHECK-NEXT:         (single_payload_enum size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
//CHECK-NEXT:           (case name=some index=0 offset=0
//CHECK-NEXT:             (struct size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))
//CHECK-NEXT:           (case name=none index=1)))))
//CHECK-NEXT:   (case name=none index=1))

//CHECK-NEXT: Mangled name: $s22reflect_nested_generic6Outer2V1EO1FO7InnererVy_AA2S1V_AA2S2V_GSg
//CHECK-NEXT: Demangled name: Swift.Optional<reflect_nested_generic.Outer2.E<reflect_nested_generic.S1>.F<reflect_nested_generic.S2>.Innerer<>>

//CHECK: Enum value:
//CHECK-NEXT: (enum_value name=none index=1)

doneReflecting()

// CHECK: Done.

