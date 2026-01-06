// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_Enum_value
// RUN: %target-codesign %t/reflect_Enum_value

// RUN: %target-run %target-swift-reflection-test %t/reflect_Enum_value | tee /dev/stderr | %FileCheck %s --check-prefix=CHECK --check-prefix=X%target-ptrsize --dump-input=fail

// REQUIRES: objc_interop
// REQUIRES: executable_test
// REQUIRES: reflection_test_support
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan

import Foundation
import SwiftReflectionTest

enum OneCaseNoPayload {
case only
}

reflect(enum: OneCaseNoPayload.only)

// CHECK: Reflecting an enum.
// CHECK-NEXT: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (enum reflect_Enum_value.OneCaseNoPayload)

// CHECK: Type info:
// CHECK-NEXT: (no_payload_enum size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-NEXT:   (case name=only index=0))

// CHECK: Enum value:
// CHECK-NEXT: (enum_value name=only index=0)

reflect(enumValue: Optional<OneCaseNoPayload>.some(.only))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (bound_generic_enum Swift.Optional
// CHECK-NEXT:   (enum reflect_Enum_value.OneCaseNoPayload))
// CHECK-NEXT: Value: .some(.only)

reflect(enumValue: Optional<Optional<OneCaseNoPayload>>.some(.some(.only)))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (bound_generic_enum Swift.Optional
// CHECK-NEXT:   (bound_generic_enum Swift.Optional
// CHECK-NEXT:     (enum reflect_Enum_value.OneCaseNoPayload)))
// CHECK-NEXT: Value: .some(.some(.only))

reflect(enumValue: Optional<Optional<OneCaseNoPayload>>.some(.none))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (bound_generic_enum Swift.Optional
// CHECK-NEXT:   (bound_generic_enum Swift.Optional
// CHECK-NEXT:     (enum reflect_Enum_value.OneCaseNoPayload)))
// CHECK-NEXT: Value: .some(.none)

struct StructInt {
var a: Int
}

enum OneStructPayload {
case payloadA(StructInt)
case cowboyAlice
case cowboyBob
case cowboyCharlie
}

reflect(enumValue: OneStructPayload.payloadA(StructInt(a: 0)))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (enum reflect_Enum_value.OneStructPayload)
// CHECK-NEXT: Value: .payloadA(_)

reflect(enumValue: OneStructPayload.cowboyCharlie)

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (enum reflect_Enum_value.OneStructPayload)
// CHECK-NEXT: Value: .cowboyCharlie

@objc class ObjCClass : NSObject {
var a: Int = 0
}

enum OneObjCPayload {
case payloadA(ObjCClass)
case otherA
case otherB
case otherC
case otherD
case otherE
}

reflect(enumValue: OneObjCPayload.payloadA(ObjCClass()))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (enum reflect_Enum_value.OneObjCPayload)
// CHECK-NEXT: Value: .payloadA(_)

reflect(enumValue: OneObjCPayload.otherC)

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (enum reflect_Enum_value.OneObjCPayload)
// CHECK-NEXT: Value: .otherC

class SwiftClass {
var a: Int = 0
}

enum OneSwiftClassPayload {
case payloadA(SwiftClass)
case otherA
case otherB
case otherC
case otherD
case otherE
}

reflect(enumValue: OneSwiftClassPayload.payloadA(SwiftClass()))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (enum reflect_Enum_value.OneSwiftClassPayload)
// CHECK-NEXT: Value: .payloadA(_)

reflect(enum: OneSwiftClassPayload.otherC)

// CHECK: Reflecting an enum.
// CHECK-NEXT: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (enum reflect_Enum_value.OneSwiftClassPayload)

// CHECK: Type info:
// X64-NEXT: (single_payload_enum size=8 alignment=8 stride=8 num_extra_inhabitants=2147483642 bitwise_takable=1
// X32-NEXT: (single_payload_enum size=4 alignment=4 stride=4 num_extra_inhabitants=4091 bitwise_takable=1
// CHECK-NEXT:   (case name=payloadA index=0 offset=0
// CHECK-NEXT:     (reference kind=strong refcounting=native))
// CHECK-NEXT:   (case name=otherA index=1)
// CHECK-NEXT:   (case name=otherB index=2)
// CHECK-NEXT:   (case name=otherC index=3)
// CHECK-NEXT:   (case name=otherD index=4)
// CHECK-NEXT:   (case name=otherE index=5))

// CHECK: Enum value:
// CHECK-NEXT: (enum_value name=otherC index=3)

reflect(enumValue: Optional<OneSwiftClassPayload>.none)

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (bound_generic_enum Swift.Optional
// CHECK-NEXT:   (enum reflect_Enum_value.OneSwiftClassPayload))
// CHECK-NEXT: Value: .none

reflect(enumValue: Optional<Optional<OneSwiftClassPayload>>.none)

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (bound_generic_enum Swift.Optional
// CHECK-NEXT:   (bound_generic_enum Swift.Optional
// CHECK-NEXT:     (enum reflect_Enum_value.OneSwiftClassPayload)))
// CHECK-NEXT: Value: .none

reflect(enumValue: Optional<Optional<OneSwiftClassPayload>>.some(.none))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (bound_generic_enum Swift.Optional
// CHECK-NEXT:   (bound_generic_enum Swift.Optional
// CHECK-NEXT:     (enum reflect_Enum_value.OneSwiftClassPayload)))
// CHECK-NEXT: Value: .some(.none)

reflect(enumValue: Optional<Optional<OneSwiftClassPayload>>.some(.some(.otherC)))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (bound_generic_enum Swift.Optional
// CHECK-NEXT:   (bound_generic_enum Swift.Optional
// CHECK-NEXT:     (enum reflect_Enum_value.OneSwiftClassPayload)))
// CHECK-NEXT: Value: .some(.some(.otherC))

reflect(enumValue: Optional<Optional<OneSwiftClassPayload>>.some(.some(.otherE)))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (bound_generic_enum Swift.Optional
// CHECK-NEXT:   (bound_generic_enum Swift.Optional
// CHECK-NEXT:     (enum reflect_Enum_value.OneSwiftClassPayload)))
// CHECK-NEXT: Value: .some(.some(.otherE))

reflect(enumValue: Optional<Optional<OneSwiftClassPayload>>.some(.some(.payloadA(SwiftClass()))))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (bound_generic_enum Swift.Optional
// CHECK-NEXT:   (bound_generic_enum Swift.Optional
// CHECK-NEXT:     (enum reflect_Enum_value.OneSwiftClassPayload)))
// CHECK-NEXT: Value: .some(.some(.payloadA(_)))

struct MixedStruct {
  var a = Int(0)
  var b = SwiftClass()
}

enum OneMixedStructPayload {
case otherA
case otherB
case payloadA(MixedStruct)
}

reflect(enumValue: OneMixedStructPayload.otherB)

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (enum reflect_Enum_value.OneMixedStructPayload)
// CHECK-NEXT: Value: .otherB

reflect(enumValue: OneMixedStructPayload.payloadA(MixedStruct()))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (enum reflect_Enum_value.OneMixedStructPayload)
// CHECK-NEXT: Value: .payloadA(_)

reflect(enumValue: Optional<OneMixedStructPayload>.none)

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (bound_generic_enum Swift.Optional
// CHECK-NEXT:   (enum reflect_Enum_value.OneMixedStructPayload))
// CHECK-NEXT: Value: .none

enum OneNestedPayload {
case cargoA(OneMixedStructPayload)
case alternateA
case alternateB
case alternateC
}

reflect(enumValue: OneNestedPayload.alternateB)

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (enum reflect_Enum_value.OneNestedPayload)
// CHECK-NEXT: Value: .alternateB


enum OneTuplePayload {
case holderA((i: Int, c: SwiftClass))
case emptyA
case emptyB
case emptyC
}

reflect(enumValue: OneTuplePayload.holderA((i: 7, c: SwiftClass())))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (enum reflect_Enum_value.OneTuplePayload)
// CHECK-NEXT: Value: .holderA(_)

reflect(enumValue: OneTuplePayload.emptyB)

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (enum reflect_Enum_value.OneTuplePayload)
// CHECK-NEXT: Value: .emptyB

func foo() -> Int { return 7; }

enum OneFunctionPayload {
case cargoA(() -> Int)
case alternateA
case alternateB
case alternateC
}

reflect(enumValue: OneFunctionPayload.cargoA(foo))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (enum reflect_Enum_value.OneFunctionPayload)
// CHECK-NEXT: Value: .cargoA(_)

reflect(enumValue: OneFunctionPayload.alternateC)

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (enum reflect_Enum_value.OneFunctionPayload)
// CHECK-NEXT: Value: .alternateC

func tester1() {
  let a = 7

  func foo() -> Int { return a; }

  enum OneClosurePayload {
  case cargoA(() -> Int)
  case alternateA
  case alternateB
  case alternateC
  }

  reflect(enumValue: OneClosurePayload.cargoA(foo))

  // CHECK: Reflecting an enum value.
  // CHECK-NEXT: Type reference:

  // XXX TODO: Figure out why the type reference is dumped differently sometimes:
  // XXXX-NEXT: (nominal with unmangled suffix
  // XXXX-NEXT: (enum OneClosurePayload #1 in reflect_Enum_value.tester1() -> ())

  // CHECK: Value: .cargoA(_)

  reflect(enumValue: OneClosurePayload.alternateB)

  // CHECK: Reflecting an enum value.
  // CHECK-NEXT: Type reference:

  // XXX TODO: Figure out why the type reference is dumped differently sometimes:
  // XXXX-NEXT: (nominal with unmangled suffix
  // XXXX-NEXT: (enum OneClosurePayload #1 in reflect_Enum_value.tester1() -> ())

  // CHECK: Value: .alternateB
}

tester1()


enum OneOptionalPayload {
case boxA(Optional<Int>)
case unboxA
case unboxB
case unboxC
case unboxD
case unboxE
}

reflect(enumValue: OneOptionalPayload.boxA(7))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (enum reflect_Enum_value.OneOptionalPayload)
// CHECK-NEXT: Value: .boxA(.some(_))

reflect(enumValue: OneOptionalPayload.unboxE)

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (enum reflect_Enum_value.OneOptionalPayload)
// CHECK-NEXT: Value: .unboxE

indirect enum OneIndirectPayload {
case child(OneIndirectPayload)
case leafA
case leafB
case leafC
case leafD
case leafE
case leafF
}

reflect(enumValue: OneIndirectPayload.child(.child(.leafF)))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (enum reflect_Enum_value.OneIndirectPayload)
// CHECK-NEXT: Value: .child(.child(.leafF))

reflect(enumValue: OneIndirectPayload.child(.leafF))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (enum reflect_Enum_value.OneIndirectPayload)
// CHECK-NEXT: Value: .child(.leafF)

reflect(enumValue: OneIndirectPayload.leafF)

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (enum reflect_Enum_value.OneIndirectPayload)
// CHECK-NEXT: Value: .leafF

enum ADT {
  case A
  case B(Int)
}

enum GuineaPig {
    case a
    indirect case b(ADT)
}

reflect(enumValue: GuineaPig.b(ADT.B(42)))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (enum reflect_Enum_value.GuineaPig)
// CHECK-NEXT: Value: .b(.B(_))

class SimpleSwiftClass {
  let value = 7
}

struct StructWrappingSimpleSwiftClass {
  let wrapped = SimpleSwiftClass()
}

enum EnumWrappingStructWrappingSimpleSwiftClass {
case payload(StructWrappingSimpleSwiftClass)
case nonpayloadA
case nonpayloadB
}

reflect(enumValue: EnumWrappingStructWrappingSimpleSwiftClass.payload(StructWrappingSimpleSwiftClass()))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (enum reflect_Enum_value.EnumWrappingStructWrappingSimpleSwiftClass)
// CHECK-NEXT: Value: .payload(_)

reflect(enumValue: Optional<EnumWrappingStructWrappingSimpleSwiftClass>.some(.payload(StructWrappingSimpleSwiftClass())))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (bound_generic_enum Swift.Optional
// CHECK-NEXT:   (enum reflect_Enum_value.EnumWrappingStructWrappingSimpleSwiftClass))
// CHECK-NEXT: Value: .some(.payload(_))

reflect(enumValue: Optional<EnumWrappingStructWrappingSimpleSwiftClass>.none)

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (bound_generic_enum Swift.Optional
// CHECK-NEXT:   (enum reflect_Enum_value.EnumWrappingStructWrappingSimpleSwiftClass))
// CHECK-NEXT: Value: .none


// XXX TODO: test enum with thin function payload XXX

doneReflecting()
// CHECK: Done.

