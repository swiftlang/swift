// RUN: %target-swift-ide-test -print-module -module-to-print=CxxConstructors -I %S/Inputs/ -source-filename=x -enable-cxx-interop | %FileCheck %s

// CHECK:      struct ExplicitDefaultConstructor {
// CHECK-NEXT:   var x: Int32
// CHECK-NEXT:   init()
// CHECK-NEXT: }
// CHECK-NEXT: struct ImplicitDefaultConstructor {
// CHECK-NEXT:   var x: Int32
// CHECK-NEXT:   init()
// CHECK-NEXT:   init(x: Int32)
// CHECK-NEXT: }
// CHECK-NEXT: struct MemberOfClassType {
// CHECK-NEXT:   var member: ImplicitDefaultConstructor
// CHECK-NEXT:   init()
// CHECK-NEXT:   init(member: ImplicitDefaultConstructor)
// CHECK-NEXT: }
// CHECK-NEXT: struct DefaultConstructorDeleted {
// CHECK-NEXT: }
// CHECK-NEXT: struct ConstructorWithParam {
// CHECK-NEXT:   var x: Int32
// CHECK-NEXT:   init(_ val: Int32)
// CHECK-NEXT: }
// CHECK-NEXT: struct Base {
// CHECK-NEXT:   init()
// CHECK-NEXT: }
// CHECK-NEXT: struct ArgType {
// CHECK-NEXT:   var i: Int32
// CHECK-NEXT:   init()
// CHECK-NEXT:   init(i: Int32)
// CHECK-NEXT: }
// CHECK-NEXT: struct HasVirtualBase {
// CHECK-NEXT:   var i: Int32
// CHECK-NEXT:   init(_ Arg: ArgType)
// CHECK-NEXT: }
