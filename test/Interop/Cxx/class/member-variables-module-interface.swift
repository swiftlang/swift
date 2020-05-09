// RUN: %target-swift-ide-test -print-module -module-to-print=MemberVariables -I %S/Inputs -source-filename=x -enable-cxx-interop | %FileCheck %s

// CHECK:      struct MyClass {
// CHECK-NEXT:   var const_member: Int32 { get }
// CHECK-NEXT:   init()
// CHECK-NEXT:   init(const_member: Int32)
// CHECK-NEXT: }
