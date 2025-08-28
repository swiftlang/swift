// RUN: %target-swift-ide-test -print-module -module-to-print=ConstexprInNamespace -I %S/Inputs -source-filename=x -cxx-interoperability-mode=upcoming-swift | %FileCheck %s

// CHECK: enum NS1 {
// CHECK:   static var myConstexprInt: Int32
// CHECK:   static var myStaticConstexprInt: Int32
// CHECK: }
