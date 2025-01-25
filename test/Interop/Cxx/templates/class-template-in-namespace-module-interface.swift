// RUN: %target-swift-ide-test -print-module -module-to-print=ClassTemplateInNamespace -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK: enum Space {
// CHECK:   @available(*, unavailable, message: "Un-specialized class templates are not currently supported. Please use a specialization of this type.")
// CHECK:   struct Ship<> {
// CHECK:   }
// CHECK:   typealias Orbiter = Space.Ship<((CBool) -> Void)>

// CHECK:   typealias IntBoxWithinNS = Space.Box<CInt>
// CHECK:   typealias BoxOfIntBoxWithinNS = Space.Box<Space.Box<CInt>>

// CHECK:   enum NestedNS1 {
// CHECK:     typealias ImplBox1 = Space.Box<Space.NestedNS1.Impl>
// CHECK:   }

// CHECK:   enum NestedNS2 {
// CHECK:     typealias ImplBox2 = Space.Box<Space.NestedNS2.Impl>
// CHECK:   }

// CHECK: }
