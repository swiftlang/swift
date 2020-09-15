// RUN: %target-swift-ide-test -print-module -module-to-print=MemberInline -I %S/Inputs -source-filename=x -enable-cxx-interop | %FileCheck %s

// CHECK: struct IntBox {
// CHECK:   static func - (lhs: inout IntBox, rhs: IntBox) -> IntBox
// CHECK: }
