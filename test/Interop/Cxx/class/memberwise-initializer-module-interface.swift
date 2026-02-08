// RUN: %target-swift-ide-test -print-module -module-to-print=MemberwiseInitializer -access-filter-public -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK:      struct StructPrivateOnly {
// CHECK-NEXT:   init()
// CHECK-NEXT: }
// CHECK-NEXT: struct StructPublicOnly {
// CHECK-NEXT:   init(varPublic: Int32)
// CHECK-NEXT:   init()
// CHECK-NEXT:   var varPublic: Int32
// CHECK-NEXT: }
// CHECK-NEXT: struct StructEmptyPrivateSection {
// CHECK-NEXT:   init(varPublic: Int32)
// CHECK-NEXT:   init()
// CHECK-NEXT:   var varPublic: Int32
// CHECK-NEXT: }
// CHECK-NEXT: struct StructPublicAndPrivate {
// CHECK-NEXT:   init()
// CHECK-NEXT:   var varPublic: Int32
// CHECK-NEXT: }
// CHECK-NEXT: struct StructWithUnimportedMemberFunction {
// CHECK-NEXT:   init(varPublic: Int32)
// CHECK-NEXT:   init()
// CHECK-NEXT:   var varPublic: Int32
// CHECK-NEXT: }
// CHECK-NEXT: struct ClassPrivateOnly {
// CHECK-NEXT:   init()
// CHECK-NEXT: }
// CHECK-NEXT: struct ClassPublicOnly {
// CHECK-NEXT:   init(varPublic: Int32)
// CHECK-NEXT:   init()
// CHECK-NEXT:   var varPublic: Int32
// CHECK-NEXT: }
// CHECK-NEXT: struct ClassEmptyPublicSection {
// CHECK-NEXT:   init()
// CHECK-NEXT: }
// CHECK-NEXT: struct ClassPrivateAndPublic {
// CHECK-NEXT:   init()
// CHECK-NEXT:   var varPublic: Int32
// CHECK-NEXT: }
// CHECK-NEXT: struct ClassWithUnimportedMemberFunction {
// CHECK-NEXT:   init(varPublic: Int32)
// CHECK-NEXT:   init()
// CHECK-NEXT:   var varPublic: Int32
// CHECK-NEXT: }
// CHECK-NEXT: struct ClassWithTemplatedFunction {
// CHECK-NEXT:   init(varPublic: Int32)
// CHECK-NEXT:   init()
// CHECK-NEXT:   var varPublic: Int32
// CHECK-NEXT: }
// CHECK-NEXT: struct ClassWithTemplatedUsingDecl {
// CHECK-NEXT:   init(varPublic: Int32)
// CHECK-NEXT:   init()
// CHECK-NEXT:   var varPublic: Int32
// CHECK-NEXT: }
// CHECK-NEXT: struct ClassWithStaticAssert {
// CHECK-NEXT:   init(x: Int32, y: Int32)
// CHECK-NEXT:   init()
// CHECK-NEXT:   var x: Int32
// CHECK-NEXT:   var y: Int32
// CHECK-NEXT: }
// CHECK-NEXT: struct ClassWithStaticAssert2 {
// CHECK-NEXT:   init(x: Int32, y: Int32)
// CHECK-NEXT:   init()
// CHECK-NEXT:   var x: Int32
// CHECK-NEXT:   var y: Int32
// CHECK-NEXT: }
