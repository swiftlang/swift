// RUN: %target-swift-ide-test -print-module -module-to-print=TemplatesWithForwardDecl -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK:      enum NS1 {
// CHECK-NEXT:   struct __CxxTemplateInstN3NS115ForwardDeclaredIiEE {
// CHECK-NEXT:     init()
// CHECK-NEXT:   }
// CHECK-NEXT:   struct ForwardDeclared<T> {
// CHECK-NEXT:   }
// CHECK-NEXT:   struct __CxxTemplateInstN3NS14DeclIiEE {
// CHECK-NEXT:     init()
// CHECK-NEXT:     init(fwd: NS1.__CxxTemplateInstN3NS115ForwardDeclaredIiEE)
// CHECK-NEXT:     typealias MyInt = Int32
// CHECK-NEXT:     var fwd: NS1.__CxxTemplateInstN3NS115ForwardDeclaredIiEE
// CHECK-NEXT:     static let intValue: NS1.__CxxTemplateInstN3NS14DeclIiEE.MyInt
// CHECK-NEXT:   }
// CHECK-NEXT:   struct Decl<T> {
// CHECK-NEXT:   }
// CHECK-NEXT:   typealias di = NS1.__CxxTemplateInstN3NS14DeclIiEE
// CHECK-NEXT: }
