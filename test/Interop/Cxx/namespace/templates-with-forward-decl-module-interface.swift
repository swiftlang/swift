// RUN: %target-swift-ide-test -print-module -module-to-print=TemplatesWithForwardDecl -I %S/Inputs -source-filename=x -enable-cxx-interop | %FileCheck %s

// CHECK: extension NS1 {
// CHECK:   struct __CxxTemplateInstN3NS14DeclIiEE {
// CHECK:     typealias MyInt = Int32
// CHECK:     var fwd: NS1.__CxxTemplateInstN3NS115ForwardDeclaredIiEE
// CHECK:     static let intValue: NS1.__CxxTemplateInstN3NS14DeclIiEE.MyInt
// CHECK:   }
// CHECK: }
