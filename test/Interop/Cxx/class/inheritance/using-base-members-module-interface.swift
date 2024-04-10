// RUN: %target-swift-ide-test -print-module -module-to-print=UsingBaseMembers -I %S/Inputs -source-filename=x -cxx-interoperability-mode=swift-6 | %FileCheck %s
// RUN: %target-swift-ide-test -print-module -module-to-print=UsingBaseMembers -I %S/Inputs -source-filename=x -cxx-interoperability-mode=upcoming-swift | %FileCheck %s

// CHECK: struct PublicBase {
// CHECK-NEXT:   init()
// CHECK-NEXT:   func publicGetter() -> Int32
// CHECK-NEXT:   mutating func publicSetter(_ v: Int32)
// CHECK-NEXT:   func notExposed()
// CHECK-NEXT:   }

// CHECK: struct PublicBasePrivateInheritance {
// CHECK-NEXT:   init()
// CHECK-NEXT:   func publicGetter() -> Int32
// CHECK-NEXT:   mutating func publicSetter(_ v: Int32)
// CHECK-NEXT: }

// CHECK: struct PublicBaseProtectedInheritance {
// CHECK-NEXT:   init()
// CHECK-NEXT:   func publicGetter() -> Int32
// CHECK-NEXT:   mutating func publicSetter(_ v: Int32)
// CHECK-NEXT: }

// CHECK: struct UsingBaseConstructorWithParam {
// CHECK-NEXT:   init(_: IntBox)
// CHECK-NEXT:   init(_: UInt32)
// CHECK-NEXT:   init(_: Int32)
// CHECK-NEXT:   var value: Int32
// CHECK-NEXT: }

// CHECK: struct UsingBaseConstructorEmpty {
// CHECK-NEXT:   init()
// CHECK-NEXT:   init(_: Empty)
// CHECK-NEXT:   var value: Int32
// CHECK-NEXT: }
