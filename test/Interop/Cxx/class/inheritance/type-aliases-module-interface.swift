// RUN: %target-swift-ide-test -print-module -module-to-print=TypeAliases -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK:      struct Base {
// CHECK-NEXT:   init()
// CHECK-NEXT:   struct Struct {
// CHECK-NEXT:     init()
// CHECK-NEXT:   }
// CHECK-NEXT:   typealias T = Int32
// CHECK-NEXT:   typealias U = Base.Struct
// CHECK-NEXT: }

// CHECK-NEXT: struct Derived {
// CHECK-NEXT:   init()
// CHECK-NEXT:   typealias Struct = Base.Struct.Type
// CHECK-NEXT:   typealias T = Int32
// CHECK-NEXT:   typealias U = Base.Struct
// CHECK-NEXT: }
