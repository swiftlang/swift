// RUN: %target-swift-ide-test -dump-importer-lookup-table -source-filename %s -import-objc-header %S/Inputs/swift_name.h -I %S/Inputs/custom-modules > %t.ide-test.log 2>&1
// RUN: %FileCheck %s < %t.ide-test.log

// RUN: %target-typecheck-verify-swift -dump-clang-lookup-tables -import-objc-header %S/Inputs/swift_name.h -I %S/Inputs/custom-modules > %t.frontend.log 2>&1
// RUN: %FileCheck %s < %t.frontend.log

// REQUIRES: objc_interop
import ImportAsMember

// CHECK-LABEL: <<ImportAsMember lookup table>>
// CHECK-NEXT: Base name -> entry mappings:
// CHECK:        Struct1:
// CHECK-NEXT:     TU: IAMStruct1
// CHECK:        init:
// CHECK-NEXT:     IAMStruct1: IAMStruct1CreateSimple
// CHECK:        radius:
// CHECK-NEXT:     IAMStruct1: IAMStruct1GetRadius, IAMStruct1SetRadius

// CHECK: Globals-as-members mapping:
// CHECK-NEXT: IAMStruct1: IAMStruct1GlobalVar, IAMStruct1CreateSimple, IAMStruct1CreateSpecialLabel, IAMStruct1Invert, IAMStruct1InvertInPlace, IAMStruct1Rotate, IAMStruct1Scale, IAMStruct1GetRadius, IAMStruct1SetRadius, IAMStruct1GetAltitude, IAMStruct1SetAltitude, IAMStruct1GetMagnitude, IAMStruct1StaticMethod, IAMStruct1StaticGetProperty, IAMStruct1StaticSetProperty, IAMStruct1StaticGetOnlyProperty, IAMStruct1SelfComesLast, IAMStruct1SelfComesThird, IAMStruct1StaticVar1, IAMStruct1StaticVar2, IAMStruct1CreateFloat, IAMStruct1GetZeroStruct1

// CHECK-LABEL: <<Bridging header lookup table>>
// CHECK-NEXT:      Base name -> entry mappings:
// CHECK-NEXT:    Bar:
// CHECK-NEXT:      TU: SNFoo
// CHECK-NEXT:    Blue:
// CHECK-NEXT:      SNColorChoice: SNColorBlue
// CHECK:    MyInt:
// CHECK-NEXT:      TU: SNIntegerType
// CHECK-NEXT:    Point:
// CHECK-NEXT:      TU: SNPoint
// CHECK:    Rouge:
// CHECK-NEXT:      SNColorChoice: SNColorRed
// CHECK:    SNColorChoice:
// CHECK-NEXT:      TU: SNColorChoice, SNColorChoice
// CHECK:    SomeStruct:
// CHECK:      TU: SNSomeStruct
// CHECK:    __SNTransposeInPlace:
// CHECK-NEXT:      TU: SNTransposeInPlace
// CHECK-NEXT:    __swift:
// CHECK-NEXT:      TU: __swift
// CHECK-NEXT:    adding:
// CHECK-NEXT:      SNSomeStruct: SNAdding
// CHECK-NEXT:    blue:
// CHECK-NEXT:      SNColorChoice: SNColorBlue
// CHECK-NEXT:    defaultValue:
// CHECK-NEXT:      SNSomeStruct: SNSomeStructGetDefault, SNSomeStructSetDefault
// CHECK-NEXT:    defaultX:
// CHECK-NEXT:      SNSomeStruct: DefaultXValue
// CHECK-NEXT:    foo:
// CHECK-NEXT:      SNSomeStruct: SNSomeStructGetFoo, SNSomeStructSetFoo
// CHECK-NEXT:    green:
// CHECK-NEXT:      SNColorChoice: SNColorGreen
// CHECK-NEXT:    init:
// CHECK-NEXT:      SNSomeStruct: SNCreate
// CHECK-NEXT:    makeSomeStruct:
// CHECK-NEXT:      TU: SNMakeSomeStruct, SNMakeSomeStructForX
// CHECK-NEXT:    x:
// CHECK-NEXT:      SNSomeStruct: X
// CHECK-NEXT:      SNPoint: x
// CHECK-NEXT:    y:
// CHECK-NEXT:      SNPoint: y
// CHECK-NEXT:    z:
// CHECK-NEXT:      SNPoint: z


// CHECK-NEXT: Globals-as-members mapping:
// CHECK-NEXT:   SNSomeStruct: DefaultXValue, SNAdding, SNCreate, SNSomeStructGetDefault, SNSomeStructSetDefault, SNSomeStructGetFoo, SNSomeStructSetFoo
