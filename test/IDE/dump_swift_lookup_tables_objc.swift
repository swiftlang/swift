// RUN: %target-swift-ide-test -dump-importer-lookup-table -source-filename %s -import-objc-header %S/Inputs/swift_name_objc.h > %t.log 2>&1
// RUN: FileCheck %s < %t.log

// REQUIRES: objc_interop

// CHECK:      Base -> full name mappings:
// CHECK-NEXT:   NSAccessibility --> NSAccessibility
// CHECK-NEXT:   NSError --> NSError
// CHECK-NEXT:   NSErrorImports --> NSErrorImports
// CHECK-NEXT:   SNCollision --> SNCollision
// CHECK-NEXT:   SNCollisionProtocol --> SNCollisionProtocol
// CHECK-NEXT:   SomeClass --> SomeClass
// CHECK-NEXT:   SomeProtocol --> SomeProtocol
// CHECK-NEXT:   UIActionSheet --> UIActionSheet
// CHECK-NEXT:   accessibilityFloat --> accessibilityFloat()
// CHECK-NEXT:   categoryMethodWithX --> categoryMethodWithX(_:y:), categoryMethodWithX(_:y:z:)
// CHECK-NEXT:   doubleProperty --> doubleProperty{{$}}
// CHECK-NEXT:   extensionMethodWithX --> extensionMethodWithX(_:y:)
// CHECK-NEXT:   floatProperty --> floatProperty{{$}}
// CHECK-NEXT:   init --> init(float:), init(withDefault:), init(double:), init(withTry:), init(uint8:), init(title:delegate:cancelButtonTitle:destructiveButtonTitle:)
// CHECK-NEXT:   instanceMethodWithX --> instanceMethodWithX(_:y:z:)
// CHECK-NEXT:   method --> method()
// CHECK-NEXT:   methodWithFloat --> methodWithFloat(_:)
// CHECK-NEXT:   protoInstanceMethodWithX --> protoInstanceMethodWithX(_:y:)
// CHECK-NEXT:   setAccessibilityFloat --> setAccessibilityFloat(_:)

// CHECK:      Full name -> entry mappings:
// CHECK-NEXT:   NSAccessibility:
// CHECK-NEXT:     TU: NSAccessibility{{$}}
// CHECK-NEXT:   NSError:
// CHECK-NEXT:     TU: NSError
// CHECK-NEXT:   NSErrorImports:
// CHECK-NEXT:     TU: NSErrorImports
// CHECK-NEXT:   SNCollision:
// CHECK-NEXT:     TU: SNCollision{{$}}
// CHECK-NEXT:   SNCollisionProtocol:
// CHECK-NEXT:     TU: SNCollision{{$}}
// CHECK-NEXT:   SomeClass:
// CHECK-NEXT:     TU: SNSomeClass
// CHECK-NEXT:   SomeProtocol:
// CHECK-NEXT:     TU: SNSomeProtocol
// CHECK-NEXT:   UIActionSheet:
// CHECK-NEXT:     TU: UIActionSheet
// CHECK-NEXT:   accessibilityFloat():
// CHECK-NEXT:     NSAccessibility: -[NSAccessibility accessibilityFloat]
// CHECK-NEXT:   categoryMethodWithX(_:y:):
// CHECK-NEXT:     SNSomeClass: -[SNSomeClass categoryMethodWithX:y:]
// CHECK-NEXT:   categoryMethodWithX(_:y:z:):
// CHECK-NEXT:     SNSomeClass: -[SNSomeClass categoryMethodWithX:y:z:]
// CHECK-NEXT:   doubleProperty:
// CHECK-NEXT:     SNSomeClass: SNSomeClass.doubleProperty
// CHECK-NEXT:   extensionMethodWithX(_:y:):
// CHECK-NEXT:     SNSomeClass: -[SNSomeClass extensionMethodWithX:y:]
// CHECK-NEXT:   floatProperty:
// CHECK-NEXT:     SNSomeClass: SNSomeClass.floatProperty
// CHECK-NEXT:   init(andReturnError:):
// CHECK-NEXT:     NSErrorImports: -[NSErrorImports initAndReturnError:]
// CHECK-NEXT:   init(double:):
// CHECK-NEXT:     SNSomeClass: +[SNSomeClass someClassWithDouble:]
// CHECK-NEXT:   init(float:):
// CHECK-NEXT:     SNSomeClass: -[SNSomeClass initWithFloat:]
// CHECK-NEXT:     NSErrorImports: -[NSErrorImports initWithFloat:error:]
// CHECK-NEXT:   init(title:delegate:cancelButtonTitle:destructiveButtonTitle:):
// CHECK-NEXT:     UIActionSheet: -[UIActionSheet initWithTitle:delegate:cancelButtonTitle:destructiveButtonTitle:otherButtonTitles:]
// CHECK-NEXT:   init(uint8:):
// CHECK-NEXT:     SNSomeClass: +[SNSomeClass buildWithUnsignedChar:]
// CHECK-NEXT:   init(withDefault:):
// CHECK-NEXT:     SNSomeClass: -[SNSomeClass initWithDefault]
// CHECK-NEXT:   init(withTry:):
// CHECK-NEXT:     SNSomeClass: +[SNSomeClass someClassWithTry:]
// CHECK-NEXT:   instanceMethodWithX(_:y:z:):
// CHECK-NEXT:     SNSomeClass: -[SNSomeClass instanceMethodWithX:y:z:]
// CHECK-NEXT:   method():
// CHECK-NEXT:     NSErrorImports: -[NSErrorImports methodAndReturnError:]
// CHECK-NEXT:   methodWithFloat(_:):
// CHECK-NEXT:     NSErrorImports: -[NSErrorImports methodWithFloat:error:]
// CHECK-NEXT:   protoInstanceMethodWithX(_:y:):
// CHECK-NEXT:     SNSomeProtocol: -[SNSomeProtocol protoInstanceMethodWithX:y:]
// CHECK-NEXT:   setAccessibilityFloat(_:):
// CHECK-NEXT:     NSAccessibility: -[NSAccessibility setAccessibilityFloat:]
