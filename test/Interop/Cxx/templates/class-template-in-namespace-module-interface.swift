// RUN: %target-swift-ide-test -print-module -module-to-print=ClassTemplateInNamespace -I %S/Inputs -source-filename=x -enable-cxx-interop | %FileCheck %s

// CHECK: enum Space {
// CHECK:   struct Ship<T> {
// CHECK:   }
// CHECK: }
// CHECK: enum Engine {
// CHECK:   struct Turbojet {
// CHECK:     init()
// CHECK:   }
// CHECK: }