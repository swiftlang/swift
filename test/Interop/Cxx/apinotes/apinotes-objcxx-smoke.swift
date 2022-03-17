// RUN: %target-swift-ide-test -print-module -module-to-print=SomeModule -I %S/Inputs -source-filename=x -enable-cxx-interop -enable-objc-interop | %FileCheck -check-prefix=CHECK-IDE-TEST %s
// RUN: %swift-frontend -c -enable-cxx-interop -enable-objc-interop -I %S/Inputs %s -o - -emit-sil | %FileCheck %s

import SomeModule

// CHECK: @objc @_inheritsConvenienceInitializers class MyClass : SomeClass
// CHECK-IDE-TEST: typealias NSSomeClass = SomeClass
// CHECK-IDE-TEST-NEXT: class SomeClass
class MyClass : SomeClass { }
