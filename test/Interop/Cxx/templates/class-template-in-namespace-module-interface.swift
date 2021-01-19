// RUN: %target-swift-ide-test -print-module -module-to-print=ClassTemplateInNamespace -I %S/Inputs -source-filename=x -enable-cxx-interop | %FileCheck %s

// CHECK: enum Space {
// CHECK:   struct __CxxTemplateInstN5Space4ShipIiEE {
// CHECK:     var t: Int32
// CHECK:     init()
// CHECK:     init(t: Int32)
// CHECK:   }
// CHECK:   struct Ship<T> {
// CHECK:   }
// CHECK:   typealias Orbiter = Space.__CxxTemplateInstN5Space4ShipIiEE
// CHECK: }
// CHECK: enum Engine {
// CHECK:   struct Turbojet {
// CHECK:     init()
// CHECK:   }
// CHECK: }