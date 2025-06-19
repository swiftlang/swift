// RUN: %target-swift-ide-test -print-module -module-to-print=TemplatesWithForwardDecl -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK:      enum NS1 {
// CHECK-NEXT:   struct ForwardDeclared<CInt> {
// CHECK-NEXT:     init()
// CHECK-NEXT:   }
// CHECK-NEXT:   @available(*, unavailable, message: "Un-specialized class templates are not currently supported. Please use a specialization of this type.")
// CHECK-NEXT:   struct ForwardDeclared<T> {
// CHECK-NEXT:   }
// CHECK-NEXT:   struct Decl<CInt> {
// CHECK-NEXT:     init()
// CHECK-NEXT:     init(fwd: NS1.ForwardDeclared<CInt>)
// CHECK-NEXT:     typealias MyInt = Int32
// CHECK-NEXT:     var fwd: NS1.ForwardDeclared<CInt>
// CHECK-NEXT:     static var intValue: NS1.Decl<CInt>.MyInt { get }
// CHECK-NEXT:   }
// CHECK-NEXT:   @available(*, unavailable, message: "Un-specialized class templates are not currently supported. Please use a specialization of this type.")
// CHECK-NEXT:   struct Decl<T> {
// CHECK-NEXT:   }
// CHECK-NEXT:   typealias di = NS1.Decl<CInt>
// CHECK-NEXT: }
