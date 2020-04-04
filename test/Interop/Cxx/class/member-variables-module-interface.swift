// RUN: %target-swift-ide-test -print-module -module-to-print=CxxMemberVariables -I %S/Inputs -source-filename=x -enable-cxx-interop | %FileCheck %s

// CHECK:      struct MyClass {
// CHECK-NEXT:   var const_member: Int32 { get }
// CHECK-NEXT:   init()
// CHECK-NEXT: }
