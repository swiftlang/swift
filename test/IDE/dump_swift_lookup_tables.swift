// RUN: %target-swift-ide-test -dump-importer-lookup-table -source-filename %s -import-objc-header %S/Inputs/swift_name.h > %t.log 2>&1
// RUN: FileCheck %s < %t.log

// REQUIRES: objc_interop

// CHECK-LABEL: <<Bridging header lookup table>>
// CHECK-NEXT:      Base name -> entry mappings:
// CHECK-NEXT:   Bar:
// CHECK-NEXT:     TU: SNFoo
// CHECK-NEXT:   Blue:
// CHECK-NEXT:     SNColorChoice: SNColorBlue
// CHECK-NEXT:   Green:
// CHECK-NEXT:     SNColorChoice: SNColorGreen
// CHECK-NEXT:   MyInt:
// CHECK-NEXT:     TU: SNIntegerType
// CHECK-NEXT:   Point:
// CHECK-NEXT:     TU: SNPoint
// CHECK-NEXT:   Rouge:
// CHECK-NEXT:     SNColorChoice: SNColorRed
// CHECK-NEXT:   SNColorChoice:
// CHECK-NEXT:     TU: SNColorChoice, SNColorChoice
// CHECK-NEXT:   SWIFT_ENUM:
// CHECK-NEXT:     TU: Macro
// CHECK-NEXT:   SWIFT_NAME:
// CHECK-NEXT:     TU: Macro
// CHECK-NEXT:   SomeStruct:
// CHECK-NEXT:     TU: SNSomeStruct
// CHECK-NEXT:   __SNTransposeInPlace:
// CHECK-NEXT:     TU: SNTransposeInPlace
// CHECK-NEXT:   __swift:
// CHECK-NEXT:     TU: __swift
// CHECK-NEXT:   makeSomeStruct:
// CHECK-NEXT:     TU: SNMakeSomeStruct, SNMakeSomeStructForX
// CHECK-NEXT:   x:
// CHECK-NEXT:     SNSomeStruct: X
// CHECK-NEXT:     SNPoint: x
// CHECK-NEXT:   y:
// CHECK-NEXT:     SNPoint: y
// CHECK-NEXT:   z:
// CHECK-NEXT:     SNPoint: z
