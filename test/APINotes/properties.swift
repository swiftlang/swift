// RUN: %empty-directory(%t)

// RUN: %target-swift-ide-test -F %S/Inputs/custom-frameworks -print-module -source-filename %s -module-to-print=APINotesFrameworkTest -function-definitions=false -print-regular-comments -swift-version 5 | %FileCheck -check-prefix=CHECK-SWIFT-5 -check-prefix=CHECK-BOTH %s

// RUN: %target-swift-ide-test -F %S/Inputs/custom-frameworks -print-module -source-filename %s -module-to-print=APINotesFrameworkTest -function-definitions=false -print-regular-comments -swift-version 4 | %FileCheck -check-prefix=CHECK-SWIFT-4 -check-prefix=CHECK-BOTH %s

// REQUIRES: objc_interop


// CHECK-BOTH-LABEL: class TestProperties : Base {

// CHECK-BOTH-DAG: func accessorsOnly() -> Any
// CHECK-BOTH-DAG: func setAccessorsOnly(_ accessorsOnly: Any)
// CHECK-BOTH-DAG: class func accessorsOnlyForClass() -> Any
// CHECK-BOTH-DAG: class func setAccessorsOnlyForClass(_ accessorsOnlyForClass: Any)

// CHECK-BOTH-DAG: func accessorsOnlyRO() -> Any
// CHECK-BOTH-DAG: func accessorsOnlyWeak() -> Any?
// CHECK-BOTH-DAG: func setAccessorsOnlyWeak(_ accessorsOnlyWeak: Any?)

// CHECK-SWIFT-5-DAG: var accessorsOnlyInVersion4: Any
// CHECK-SWIFT-5-DAG: class var accessorsOnlyForClassInVersion4: Any
// CHECK-SWIFT-4-DAG: func accessorsOnlyInVersion4() -> Any
// CHECK-SWIFT-4-DAG: func setAccessorsOnlyInVersion4(_ accessorsOnlyInVersion4: Any)
// CHECK-SWIFT-4-DAG: class func accessorsOnlyForClassInVersion4() -> Any
// CHECK-SWIFT-4-DAG: class func setAccessorsOnlyForClassInVersion4(_ accessorsOnlyForClassInVersion4: Any)

// CHECK-SWIFT-5-DAG: func accessorsOnlyExceptInVersion4() -> Any
// CHECK-SWIFT-5-DAG: func setAccessorsOnlyExceptInVersion4(_ accessorsOnlyExceptInVersion4: Any)
// CHECK-SWIFT-5-DAG: class func accessorsOnlyForClassExceptInVersion4() -> Any
// CHECK-SWIFT-5-DAG: class func setAccessorsOnlyForClassExceptInVersion4(_ accessorsOnlyForClassExceptInVersion4: Any)
// CHECK-SWIFT-4-DAG: var accessorsOnlyExceptInVersion4: Any
// CHECK-SWIFT-4-DAG: class var accessorsOnlyForClassExceptInVersion4: Any

// CHECK-BOTH: {{^}$}}

// CHECK-BOTH-LABEL: class TestPropertiesSub : TestProperties {
// CHECK-BOTH-DAG: func accessorsOnly() -> Any
// CHECK-BOTH-DAG: func setAccessorsOnly(_ accessorsOnly: Any)
// CHECK-BOTH-DAG: class func accessorsOnlyForClass() -> Any
// CHECK-BOTH-DAG: class func setAccessorsOnlyForClass(_ accessorsOnlyForClass: Any)
// CHECK-BOTH: {{^}$}}

// CHECK-BOTH-DAG: func accessorsOnlyWithNewType() -> Base
// CHECK-BOTH-DAG: func setAccessorsOnlyWithNewType(_ accessorsOnlyWithNewType: Base)

// CHECK-BOTH: {{^}$}}

// CHECK-SWIFT-4-DAG: func renamedAndRetyped() -> Any{{$}}
// CHECK-SWIFT-4-DAG: func setRenamedAndRetyped(_ accessorsOnlyRenamedRetyped: Any?)
// CHECK-SWIFT-5-DAG: var accessorsOnlyRenamedRetyped: Any!

// CHECK-SWIFT-4-DAG: class func renamedAndRetypedClass() -> Any{{$}}
// CHECK-SWIFT-4-DAG: class func setRenamedAndRetypedClass(_ accessorsOnlyRenamedRetypedClass: Any?)
// CHECK-SWIFT-5-DAG: class var accessorsOnlyRenamedRetypedClass: Any!

// CHECK-BOTH: {{^}$}}
