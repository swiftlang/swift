// RUN: %target-swift-ide-test -print-module -module-to-print=LinkedRecords -I %S/Inputs/ -source-filename=x -enable-cxx-interop | %FileCheck %s

// CHECK: enum Space {
// CHECK:   struct C {
// CHECK:     struct D {
// CHECK:       init(B: Space.A.B)
// CHECK:       var B: Space.A.B
// CHECK:     }
// CHECK:   }
// CHECK:   struct A {
// CHECK:     init()
// CHECK:     struct B {
// CHECK:       init(_: Int32)
// CHECK:       init(_: CChar)
// CHECK:     }
// CHECK:   }
// CHECK:   struct E {
// CHECK:     init()
// CHECK:     static func test(_: UnsafePointer<Space.C>!)
// CHECK:   }
// CHECK: }

// CHECK: struct M {
// CHECK:   init()
// CHECK: }
// CHECK: struct F {
// CHECK:   init()
// CHECK:   init(_ __Anonymous_field0: F.__Unnamed_union___Anonymous_field0, m2: M)
// CHECK:   struct __Unnamed_union___Anonymous_field0 {
// CHECK:     init()
// CHECK:     init(c: F.__Unnamed_union___Anonymous_field0.__Unnamed_struct_c)
// CHECK:     init(m: M)
// CHECK:     struct __Unnamed_struct_c {
// CHECK:       init()
// CHECK:     }
// CHECK:     var c: F.__Unnamed_union___Anonymous_field0.__Unnamed_struct_c
// CHECK:     var m: M
// CHECK:   }
// CHECK:   var __Anonymous_field0: F.__Unnamed_union___Anonymous_field0
// CHECK:   var c: F.__Unnamed_union___Anonymous_field0.__Unnamed_struct_c
// CHECK:   var m: M
// CHECK:   var m2: M
// CHECK: }
