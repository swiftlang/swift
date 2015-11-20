// RUN: %target-swift-ide-test -dump-importer-lookup-table -source-filename %s -import-objc-header %S/Inputs/swift_name.h > %t.log 2>&1
// RUN: FileCheck %s < %t.log

// CHECK:      Base -> full name mappings:
// CHECK-NEXT:   Bar --> Bar
// CHECK-NEXT:   SomeStruct --> SomeStruct
// CHECK-NEXT:   __SNTransposeInPlace --> __SNTransposeInPlace
// CHECK-NEXT:   makeSomeStruct --> makeSomeStruct(x:y:), makeSomeStruct(x:)

// CHECK:      Full name -> entry mappings:
// CHECK-NEXT:   Bar:
// CHECK-NEXT:     TU: SNFoo
// CHECK-NEXT:   SomeStruct:
// CHECK-NEXT:     TU: SNSomeStruct
// CHECK-NEXT:   __SNTransposeInPlace:
// CHECK-NEXT:     TU: SNTransposeInPlace
// CHECK-NEXT:   makeSomeStruct(x:):
// CHECK-NEXT:     TU: SNMakeSomeStructForX
// CHECK-NEXT:   makeSomeStruct(x:y:):
// CHECK-NEXT:     TU: SNMakeSomeStruct
