// RUN: %target-swift-ide-test -print-module -module-to-print=ExternWithinNamespace -I %S/Inputs -source-filename=x -cxx-interoperability-mode=upcoming-swift | %FileCheck %s

// CHECK:      enum Outer {
// CHECK-NEXT:   enum Inner {
// CHECK-NEXT:     static func foobar()
// CHECK-NEXT:     struct NestedType {
// CHECK:          }
// CHECK-NEXT:   }
// CHECK-NEXT:   enum InnerInline {
// CHECK-NEXT:     static func baz() -> Int32
// CHECK-NEXT:   }
// CHECK-NEXT: }
// CHECK-NEXT: enum ExternWithinExtern {
// CHECK-NEXT:   enum Inner {
// CHECK-NEXT:     static func deep() -> Int32
// CHECK-NEXT:   }
// CHECK-NEXT: }
