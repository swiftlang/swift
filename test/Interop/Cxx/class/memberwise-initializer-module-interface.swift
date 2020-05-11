// RUN: %target-swift-ide-test -print-module -module-to-print=MemberwiseInitializer -I %S/Inputs -source-filename=x -enable-cxx-interop | %FileCheck %s

// CHECK:      struct StructPrivateOnly {
// CHECK-NEXT:   init()
// CHECK-NEXT: }
// CHECK-NEXT: struct StructPublicOnly {
// CHECK-NEXT:   var varPublic: Int32
// CHECK-NEXT:   init()
// CHECK-NEXT:   init(varPublic: Int32)
// CHECK-NEXT: }
// CHECK-NEXT: struct StructEmptyPrivateSection {
// CHECK-NEXT:   var varPublic: Int32
// CHECK-NEXT:   init()
// CHECK-NEXT:   init(varPublic: Int32)
// CHECK-NEXT: }
// CHECK-NEXT: struct StructPublicAndPrivate {
// CHECK-NEXT:   var varPublic: Int32
// CHECK-NEXT:   init()
// CHECK-NEXT: }
// CHECK-NEXT: struct StructWithUnimportedMemberFunction {
// CHECK-NEXT:   var varPublic: Int32
// CHECK-NEXT:   init()
// CHECK-NEXT:   init(varPublic: Int32)
// CHECK-NEXT: }
// CHECK-NEXT: struct ClassPrivateOnly {
// CHECK-NEXT:   init()
// CHECK-NEXT: }
// CHECK-NEXT: struct ClassPublicOnly {
// CHECK-NEXT:   var varPublic: Int32
// CHECK-NEXT:   init()
// CHECK-NEXT:   init(varPublic: Int32)
// CHECK-NEXT: }
// CHECK-NEXT: struct ClassEmptyPublicSection {
// CHECK-NEXT:   init()
// CHECK-NEXT: }
// CHECK-NEXT: struct ClassPrivateAndPublic {
// CHECK-NEXT:   var varPublic: Int32
// CHECK-NEXT:   init()
// CHECK-NEXT: }
// CHECK-NEXT: struct ClassWithUnimportedMemberFunction {
// CHECK-NEXT:   var varPublic: Int32
// CHECK-NEXT:   init()
// CHECK-NEXT:   init(varPublic: Int32)
// CHECK-NEXT: }
