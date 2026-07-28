// RUN: %target-swift-ide-test -print-module -module-to-print=StaticMemberVar -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK: struct WithStaticAndInstanceMember {
// CHECK-NEXT:   init(myInstance: CInt)
// CHECK-NEXT:   init()
// CHECK-NEXT:   var myInstance: CInt
// CHECK-NEXT:   static var myStatic: CInt
// CHECK-NEXT: }
