// RUN: %target-swift-ide-test -print-module -module-to-print=SomeModule -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop -enable-objc-interop | %FileCheck -check-prefix=CHECK-IDE-TEST %s
// RUN: %swift-frontend -c -enable-experimental-cxx-interop -enable-objc-interop -I %S/Inputs %s -o - -emit-sil | %FileCheck %s

import SomeModule

// CHECK: @objc @_inheritsConvenienceInitializers class MyClass : SomeClass
// CHECK-IDE-TEST: typealias NSSomeClass = SomeClass
// CHECK-IDE-TEST-NEXT: class SomeClass
class MyClass : SomeClass { }

// CHECK-IDE-TEST: extension SomeClass {
// CHECK-IDE-TEST-NEXT: class func didMove(toParent parent
// CHECK-IDE-TEST-NEXT: func didMove(toParent parent:
// CHECK-IDE-TEST-NEXT: @available
// CHECK-IDE-TEST-NEXT: class func didMoveToParentViewController(_ parent
// CHECK-IDE-TEST-NEXT: @available
// CHECK-IDE-TEST-NEXT: func didMoveToParentViewController(_ parent

// CHECK: didMove
let a = SomeClass()
let b = MyClass()
let c = b!.didMove(toParent: a!)
