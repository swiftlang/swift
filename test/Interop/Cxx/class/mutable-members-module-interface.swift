// RUN: %target-swift-ide-test -print-module -module-to-print=MutableMembers -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK: struct HasPublicMutableMember {
// CHECK:   var a: CInt
// CHECK:   func foo() -> CInt
// CHECK: }

// CHECK: struct HasPrivateMutableMember {
// CHECK:   func bar()
// CHECK: }
