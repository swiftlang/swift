// RUN: %target-swift-ide-test -print-module -module-to-print=ImportAsMember -I %S/Inputs -source-filename=x -cxx-interoperability-mode=upcoming-swift | %FileCheck %s

// CHECK:      extension MyNS.NestedStruct {
// CHECK-NEXT:   func method() -> Int32
// CHECK-NEXT:   func methodConstRef() -> Int32
// CHECK-NEXT:   func methodInline() -> Int32
// CHECK-NEXT:   func nestedMethod() -> Int32
// CHECK-NEXT:   func nestedMethodInline() -> Int32
// CHECK-NEXT: }

// CHECK:      extension MyNS.MyDeepNS.DeepNestedStruct {
// CHECK-NEXT:   func method() -> Int32
// CHECK-NEXT:   func methodConstRef() -> Int32
// CHECK-NEXT:   func methodTypedef() -> Int32
// CHECK-NEXT:   func methodTypedefQualName() -> Int32
// CHECK-NEXT: }
