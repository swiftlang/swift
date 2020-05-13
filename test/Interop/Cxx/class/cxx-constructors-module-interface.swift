// RUN: %target-swift-ide-test -print-module -module-to-print=CxxConstructors -I %S/Inputs/ -source-filename=x -enable-cxx-interop | %FileCheck %s

// CHECK:      struct ExplicitDefaultConstructor {
// CHECK-NEXT:   var x: Int32
// CHECK-NEXT:   init()
// CHECK-NEXT: }
// CHECK-NEXT: struct ImplicitDefaultConstructor {
// CHECK-NEXT:   var x: Int32
// CHECK-NEXT:   init()
// CHECK-NEXT: }
// CHECK-NEXT: struct MemberOfClassType {
// CHECK-NEXT:   var member: ImplicitDefaultConstructor
// CHECK-NEXT:   init()
// CHECK-NEXT: }
// CHECK-NEXT: struct DefaultConstructorDeleted {
// CHECK-NEXT: }
// CHECK-NEXT: struct ConstructorWithParam {
// CHECK-NEXT:   var x: Int32
// CHECK-NEXT:   init(_ val: Int32)
// CHECK-NEXT: }
