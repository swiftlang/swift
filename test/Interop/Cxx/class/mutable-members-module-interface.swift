// RUN: %target-swift-ide-test -print-module -module-to-print=MutableMembers -I %S/Inputs -source-filename=x -enable-cxx-interop | %FileCheck %s

// CHECK: struct HasPublicMutableMember {
// CHECK:   var a: Int32
// CHECK:   mutating func foo() -> Int32
// CHECK: }

// CHECK: struct HasPrivateMutableMember {
// CHECK:   mutating func bar()
// CHECK: }
