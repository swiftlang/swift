// RUN: rm -rf %t && mkdir -p %t

// RUN: %target-swift-ide-test -F %S/Inputs/custom-frameworks -print-module -source-filename %s -module-to-print=APINotesFrameworkTest -function-definitions=false -print-regular-comments -swift-version 4 | %FileCheck -check-prefix=CHECK-SWIFT-4 -check-prefix=CHECK-BOTH %s

// RUN: %target-swift-ide-test -F %S/Inputs/custom-frameworks -print-module -source-filename %s -module-to-print=APINotesFrameworkTest -function-definitions=false -print-regular-comments -swift-version 3 | %FileCheck -check-prefix=CHECK-SWIFT-3 -check-prefix=CHECK-BOTH %s

// REQUIRES: objc_interop


// CHECK-BOTH-LABEL: class TestProperties : Base {

// CHECK-BOTH-DAG: func accessorsOnly() -> Any
// CHECK-BOTH-DAG: func setAccessorsOnly(_ accessorsOnly: Any)
// CHECK-BOTH-DAG: class func accessorsOnlyForClass() -> Any
// CHECK-BOTH-DAG: class func setAccessorsOnlyForClass(_ accessorsOnlyForClass: Any)

// CHECK-BOTH-DAG: func accessorsOnlyRO() -> Any
// CHECK-BOTH-DAG: func accessorsOnlyWeak() -> Any?
// CHECK-BOTH-DAG: func setAccessorsOnlyWeak(_ accessorsOnlyWeak: Any?)

// CHECK-SWIFT-4-DAG: var accessorsOnlyInVersion3: Any
// CHECK-SWIFT-4-DAG: class var accessorsOnlyForClassInVersion3: Any
// CHECK-SWIFT-3-DAG: func accessorsOnlyInVersion3() -> Any
// CHECK-SWIFT-3-DAG: func setAccessorsOnlyInVersion3(_ accessorsOnlyInVersion3: Any)
// CHECK-SWIFT-3-DAG: class func accessorsOnlyForClassInVersion3() -> Any
// CHECK-SWIFT-3-DAG: class func setAccessorsOnlyForClassInVersion3(_ accessorsOnlyForClassInVersion3: Any)

// CHECK-SWIFT-4-DAG: func accessorsOnlyExceptInVersion3() -> Any
// CHECK-SWIFT-4-DAG: func setAccessorsOnlyExceptInVersion3(_ accessorsOnlyExceptInVersion3: Any)
// CHECK-SWIFT-4-DAG: class func accessorsOnlyForClassExceptInVersion3() -> Any
// CHECK-SWIFT-4-DAG: class func setAccessorsOnlyForClassExceptInVersion3(_ accessorsOnlyForClassExceptInVersion3: Any)
// CHECK-SWIFT-3-DAG: var accessorsOnlyExceptInVersion3: Any
// CHECK-SWIFT-3-DAG: class var accessorsOnlyForClassExceptInVersion3: Any

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

// CHECK-SWIFT-3-DAG: func renamedAndRetyped() -> Any{{$}}
// CHECK-SWIFT-3-DAG: func setRenamedAndRetyped(_ accessorsOnlyRenamedRetyped: Any?)
// CHECK-SWIFT-4-DAG: var accessorsOnlyRenamedRetyped: Any!

// CHECK-SWIFT-3-DAG: class func renamedAndRetypedClass() -> Any{{$}}
// CHECK-SWIFT-3-DAG: class func setRenamedAndRetypedClass(_ accessorsOnlyRenamedRetypedClass: Any?)
// CHECK-SWIFT-4-DAG: class var accessorsOnlyRenamedRetypedClass: Any!

// CHECK-BOTH: {{^}$}}
