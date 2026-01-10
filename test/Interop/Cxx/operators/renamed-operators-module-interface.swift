// RUN: %target-swift-ide-test -print-module -module-to-print=RenamedOperators -I %S/Inputs -source-filename=x -cxx-interoperability-mode=upcoming-swift | %FileCheck %s

// CHECK: struct HasRenamedOperatorStar {
// CHECK-NOT: prefix static func * (lhs: HasRenamedOperatorStar)
// CHECK:   func dereference() -> UnsafePointer<Int32>
// CHECK: }

// CHECK: struct HasRenamedOperatorPlusPlus {
// CHECK-NOT: prefix static func ++ (lhs: HasRenamedOperatorPlusPlus)
// CHECK:   mutating func plusPlus() -> UnsafeMutablePointer<HasRenamedOperatorPlusPlus>
// CHECK: }
