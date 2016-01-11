// RUN: %target-swift-ide-test -dump-importer-lookup-table -source-filename %s -import-objc-header %S/Inputs/swift_name_objc.h > %t.log 2>&1
// RUN: FileCheck %s < %t.log

// RUN: %target-swift-ide-test -dump-importer-lookup-table -source-filename %s -import-objc-header %S/Inputs/swift_name_objc.h -enable-omit-needless-words > %t-omit-needless-words.log 2>&1
// RUN: FileCheck -check-prefix=CHECK-OMIT-NEEDLESS-WORDS %s < %t-omit-needless-words.log

// REQUIRES: objc_interop

// CHECK-LABEL: <<Foundation lookup table>>
// CHECK: Categories:{{.*}}NSValue(NSValueCreation){{.*}}

// CHECK-LABEL: <<ObjectiveC lookup table>>
// CHECK-NEXT: Base name -> entry mappings:
// CHECK-NOT: lookup table
// CHECK:   NSObject:
// CHECK-NEXT:     TU: NSObject
// CHECK-NEXT:   NSObjectProtocol:
// CHECK-NEXT:     TU: NSObject

// CHECK-LABEL: <<Bridging header lookup table>>
// CHECK-NEXT:      Base name -> entry mappings:
// CHECK-NEXT:   CCItem:
// CHECK-NEXT:     TU: CCItemRef
// CHECK-NEXT:   CCItemRef:
// CHECK-NEXT:     TU: CCItemRef
// CHECK-NEXT:   CFTypeRef:
// CHECK-NEXT:     TU: CFTypeRef
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
// CHECK-NEXT:   SWIFT_ENUM:
// CHECK-NEXT:     TU: Macro
// CHECK-NEXT:   SWIFT_NAME:
// CHECK-NEXT:     TU: Macro
// CHECK-NEXT:   SomeClass:
// CHECK-NEXT:     TU: SNSomeClass
// CHECK-NEXT:   SomeProtocol:
// CHECK-NEXT:     TU: SNSomeProtocol
// CHECK-NEXT:   UIActionSheet:
// CHECK-NEXT:     TU: UIActionSheet
// CHECK-NEXT:   __CCItem:
// CHECK-NEXT:     TU: __CCItem
// CHECK-NEXT:   __swift:
// CHECK-NEXT:     TU: __swift
// CHECK-NEXT:   accessibilityFloat:
// CHECK-NEXT:     NSAccessibility: -[NSAccessibility accessibilityFloat]
// CHECK-NEXT:   categoryMethodWithX:
// CHECK-NEXT:     SNSomeClass: -[SNSomeClass categoryMethodWithX:y:], -[SNSomeClass categoryMethodWithX:y:z:]
// CHECK-NEXT:   doubleProperty:
// CHECK-NEXT:     SNSomeClass: SNSomeClass.doubleProperty
// CHECK-NEXT:   extensionMethodWithX:
// CHECK-NEXT:     SNSomeClass: -[SNSomeClass extensionMethodWithX:y:]
// CHECK-NEXT:   floatProperty:
// CHECK-NEXT:     SNSomeClass: SNSomeClass.floatProperty
// CHECK-NEXT:   init:
// CHECK-NEXT:     SNSomeClass: -[SNSomeClass initWithFloat:], -[SNSomeClass initWithDefault], +[SNSomeClass someClassWithDouble:], +[SNSomeClass someClassWithTry:], +[SNSomeClass buildWithUnsignedChar:]
// CHECK-NEXT:     UIActionSheet: -[UIActionSheet initWithTitle:delegate:cancelButtonTitle:destructiveButtonTitle:otherButtonTitles:]
// CHECK-NEXT:     NSErrorImports: -[NSErrorImports initAndReturnError:], -[NSErrorImports initWithFloat:error:]
// CHECK-NEXT:   instanceMethodWithX:
// CHECK-NEXT:     SNSomeClass: -[SNSomeClass instanceMethodWithX:Y:Z:]
// CHECK-NEXT:   method:
// CHECK-NEXT:     NSErrorImports: -[NSErrorImports methodAndReturnError:]
// CHECK-NEXT:   methodWithFloat:
// CHECK-NEXT:     NSErrorImports: -[NSErrorImports methodWithFloat:error:]
// CHECK-NEXT:   objectAtIndexedSubscript:
// CHECK-NEXT:     SNSomeClass: -[SNSomeClass objectAtIndexedSubscript:]
// CHECK-NEXT:   optSetter:
// CHECK-NEXT:     SNCollision: SNCollision.optSetter
// CHECK-NEXT:   protoInstanceMethodWithX:
// CHECK-NEXT:     SNSomeProtocol: -[SNSomeProtocol protoInstanceMethodWithX:y:]
// CHECK-NEXT:   reqSetter:
// CHECK-NEXT:     SNCollision: SNCollision.reqSetter
// CHECK-NEXT:   setAccessibilityFloat:
// CHECK-NEXT:     NSAccessibility: -[NSAccessibility setAccessibilityFloat:]
// CHECK-NEXT:   subscript:
// CHECK-NEXT:     SNSomeClass: -[SNSomeClass objectAtIndexedSubscript:]

// CHECK: Categories: SNSomeClass(), SNSomeClass(Category1)

// CHECK-OMIT-NEEDLESS-WORDS: <<ObjectiveC lookup table>>
// CHECK-OMIT-NEEDLESS-WORDS-NOT: lookup table
// CHECK-OMIT-NEEDLESS-WORDS: respondsTo:
// CHECK-OMIT-NEEDLESS-WORDS-NEXT:     -[NSObject respondsToSelector:]

// CHECK-OMIT-NEEDLESS-WORDS: Base name -> entry mappings:
// CHECK-OMIT-NEEDLESS-WORDS:   methodWith:
// CHECK-OMIT-NEEDLESS-WORDS:     NSErrorImports: -[NSErrorImports methodWithFloat:error:]
