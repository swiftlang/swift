// RUN: %target-swift-ide-test -print-module -module-to-print=MutableMembers -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop -Xcc -DUSE_MUTATING | %FileCheck %s

// CHECK: struct HasPublicMutableMember {
// CHECK:   var a: CInt
// CHECK:   mutating func foo() -> CInt
// CHECK: }

// CHECK: struct HasPrivateMutableMember {
// CHECK:   mutating func bar()
// CHECK: }
