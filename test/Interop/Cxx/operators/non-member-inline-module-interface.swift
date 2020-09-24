// RUN: %target-swift-ide-test -print-module -module-to-print=NonMemberInline -I %S/Inputs -source-filename=x -enable-cxx-interop | %FileCheck %s

// CHECK:      func + (lhs: IntBox, rhs: IntBox) -> IntBox
// CHECK-NEXT: func - (lhs: IntBox, rhs: IntBox) -> IntBox
// CHECK-NEXT: func * (lhs: IntBox, rhs: IntBox) -> IntBox
// CHECK-NEXT: func / (lhs: IntBox, rhs: IntBox) -> IntBox
// CHECK-NEXT: func % (lhs: IntBox, rhs: IntBox) -> IntBox
// CHECK-NEXT: func & (lhs: IntBox, rhs: IntBox) -> IntBox
// CHECK-NEXT: func | (lhs: IntBox, rhs: IntBox) -> IntBox
// CHECK-NEXT: func << (lhs: IntBox, rhs: IntBox) -> IntBox
// CHECK-NEXT: func >> (lhs: IntBox, rhs: IntBox) -> IntBox
// CHECK-NEXT: func < (lhs: IntBox, rhs: IntBox) -> Bool
// CHECK-NEXT: func > (lhs: IntBox, rhs: IntBox) -> Bool
// CHECK-NEXT: func == (lhs: IntBox, rhs: IntBox) -> Bool
// CHECK-NEXT: func != (lhs: IntBox, rhs: IntBox) -> Bool
// CHECK-NEXT: func <= (lhs: IntBox, rhs: IntBox) -> Bool
// CHECK-NEXT: func >= (lhs: IntBox, rhs: IntBox) -> Bool

// CHECK:      func && (lhs: BoolBox, rhs: BoolBox) -> BoolBox
// CHECK-NEXT: func || (lhs: BoolBox, rhs: BoolBox) -> BoolBox
