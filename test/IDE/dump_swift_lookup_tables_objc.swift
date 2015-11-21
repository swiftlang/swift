// RUN: %target-swift-ide-test -dump-importer-lookup-table -source-filename %s -import-objc-header %S/Inputs/swift_name_objc.h > %t.log 2>&1
// RUN: FileCheck %s < %t.log

// REQUIRES: objc_interop

// CHECK:      Base -> full name mappings:
// CHECK-NEXT:   SNCollision --> SNCollision
// CHECK-NEXT:   SomeClass --> SomeClass
// CHECK-NEXT:   SomeProtocol --> SomeProtocol
// CHECK-NEXT:   categoryMethodWithX --> categoryMethodWithX(_:y:), categoryMethodWithX(_:y:z:)
// CHECK-NEXT:   doubleProperty --> doubleProperty, doubleProperty()
// CHECK-NEXT:   extensionMethodWithX --> extensionMethodWithX(_:y:)
// CHECK-NEXT:   floatProperty --> floatProperty, floatProperty()
// CHECK-NEXT:   initWithFloat --> initWithFloat(_:)
// CHECK-NEXT:   instanceMethodWithX --> instanceMethodWithX(_:y:z:)
// CHECK-NEXT:   protoInstanceMethodWithX --> protoInstanceMethodWithX(_:y:)
// CHECK-NEXT:   setDoubleProperty --> setDoubleProperty(_:)
// CHECK-NEXT:   someClassWithDouble --> someClassWithDouble(_:)

// CHECK:      Full name -> entry mappings:
// CHECK-NEXT:   SNCollision:
// CHECK-NEXT:     TU: SNCollision, SNCollision
// CHECK-NEXT:   SomeClass:
// CHECK-NEXT:     TU: SNSomeClass
// CHECK-NEXT:   SomeProtocol:
// CHECK-NEXT:     TU: SNSomeProtocol
// CHECK-NEXT:   categoryMethodWithX(_:y:):
// CHECK-NEXT:     SNSomeClass: -[SNSomeClass categoryMethodWithX:y:]
// CHECK-NEXT:   categoryMethodWithX(_:y:z:):
// CHECK-NEXT:     SNSomeClass: -[SNSomeClass categoryMethodWithX:y:z:]
// CHECK-NEXT:   doubleProperty:
// CHECK-NEXT:     SNSomeClass: SNSomeClass.doubleProperty
// CHECK-NEXT:   doubleProperty():
// CHECK-NEXT:     SNSomeClass: -[SNSomeClass doubleProperty]
// CHECK-NEXT:   extensionMethodWithX(_:y:):
// CHECK-NEXT:     SNSomeClass: -[SNSomeClass extensionMethodWithX:y:]
// CHECK-NEXT:   floatProperty:
// CHECK-NEXT:     SNSomeClass: SNSomeClass.floatProperty
// CHECK-NEXT:   floatProperty():
// CHECK-NEXT:     SNSomeClass: -[SNSomeClass floatProperty]
// CHECK-NEXT:   initWithFloat(_:):
// CHECK-NEXT:     SNSomeClass: -[SNSomeClass initWithFloat:]
// CHECK-NEXT:   instanceMethodWithX(_:y:z:):
// CHECK-NEXT:     SNSomeClass: -[SNSomeClass instanceMethodWithX:y:z:]
// CHECK-NEXT:   protoInstanceMethodWithX(_:y:):
// CHECK-NEXT:     SNSomeProtocol: -[SNSomeProtocol protoInstanceMethodWithX:y:]
// CHECK-NEXT:   setDoubleProperty(_:):
// CHECK-NEXT:     SNSomeClass: -[SNSomeClass setDoubleProperty:]
// CHECK-NEXT:   someClassWithDouble(_:):
// CHECK-NEXT:     SNSomeClass: +[SNSomeClass someClassWithDouble:]
