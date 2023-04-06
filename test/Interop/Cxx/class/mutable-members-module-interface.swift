// RUN: %target-swift-ide-test -print-module -module-to-print=MutableMembers -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK: struct HasPublicMutableMember {
// CHECK:   func foo() -> Int32
// CHECK:   var a: Int32
// CHECK: }

// CHECK: struct HasPrivateMutableMember {
// CHECK:   func bar()
// CHECK: }
