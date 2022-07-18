// RUN: %target-swift-ide-test -print-module -module-to-print=StaticMemberVar -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK: struct WithStaticAndInstanceMember {
// CHECK-NEXT:   init()
// CHECK-NEXT:   init(myInstance: Int32)
// CHECK-NEXT:   static var myStatic: Int32
// CHECK-NEXT:   var myInstance: Int32
// CHECK-NEXT: }
