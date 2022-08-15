// RUN: %target-swift-ide-test -print-module -module-to-print=ClassTemplateInNamespace -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK: enum Space {
// CHECK:   struct __CxxTemplateInstN5Space4ShipIJFvbEEEE {
// CHECK:     init()
// CHECK:   }
// CHECK:   struct Ship<> {
// CHECK:   }
// CHECK:   typealias Orbiter = Space.__CxxTemplateInstN5Space4ShipIJFvbEEEE
// CHECK: }
