// RUN: %target-swift-ide-test -print-module -module-to-print=ImportAsMember -I %S/Inputs -source-filename=x -cxx-interoperability-mode=upcoming-swift | %FileCheck %s

// CHECK:      extension MyNS.NestedStruct {
// CHECK-NEXT:   func method() -> CInt
// CHECK-NEXT:   func methodConstRef() -> CInt
// CHECK-NEXT:   func methodInline() -> CInt
// CHECK-NEXT:   func nestedMethod() -> CInt
// CHECK-NEXT:   func nestedMethodInline() -> CInt
// CHECK-NEXT: }

// CHECK:      extension MyNS.MyDeepNS.DeepNestedStruct {
// CHECK-NEXT:   func method() -> CInt
// CHECK-NEXT:   func methodConstRef() -> CInt
// CHECK-NEXT:   func methodTypedef() -> CInt
// CHECK-NEXT:   func methodTypedefQualName() -> CInt
// CHECK-NEXT: }
