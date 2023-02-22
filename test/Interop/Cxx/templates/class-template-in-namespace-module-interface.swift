// RUN: %target-swift-ide-test -print-module -module-to-print=ClassTemplateInNamespace -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK: enum Space {
// CHECK:   struct Ship<_> {
// CHECK:     init()
// CHECK:   }
// CHECK:   @available(*, unavailable, message: "Un-specialized class templates are not currently supported. Please use a specialization of this type.")
// CHECK:   struct Ship<> {
// CHECK:   }
// CHECK:   typealias Orbiter = Space.Ship<_>
// CHECK: }
