// RUN: %target-swift-ide-test -print-module -module-to-print=MemberVariablesNoDiagnostics -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK:      struct MyClass {
// CHECK-NEXT:   init(const_member: CInt)
// CHECK-NEXT:   init()
// CHECK-NEXT:   var const_member: CInt { get }
// CHECK-NEXT: }
