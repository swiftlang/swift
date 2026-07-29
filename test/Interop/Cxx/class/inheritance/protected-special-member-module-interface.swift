// RUN: %target-swift-ide-test -print-module -module-to-print=ProtectedSpecialMember -I %S/Inputs -source-filename=x -cxx-interoperability-mode=default | %FileCheck %s

// CHECK-NOT: ProtectedDtor
// CHECK-NOT: ProtectedDtorField
// CHECK-NOT: ProtectedCopy
// CHECK-NOT: ProtectedMove

// CHECK:      struct InheritsProtectedDtor {
// CHECK-NEXT:   init()
// CHECK-NEXT:   var inDerived: CInt
// CHECK-NEXT: }

// CHECK:      struct PrivatelyInheritsProtectedDtor {
// CHECK-NEXT:   init()
// CHECK-NEXT:   var inDerived: CInt
// CHECK-NEXT:   mutating func setFromBase(_ v: CInt)
// CHECK-NEXT: }

// CHECK:      struct InheritsProtectedCopy {
// CHECK-NEXT:   init()
// CHECK-NEXT:   func getFromBase() -> CInt
// CHECK-NEXT:   mutating func setFromBase(_ x: CInt)
// CHECK-NEXT: }

// CHECK:      struct PrivatelyInheritsProtectedCopy {
// CHECK-NEXT:   init()
// CHECK-NEXT:   func getFromBase() -> CInt
// CHECK-NEXT:   mutating func setFromBase(_ x: CInt)
// CHECK-NEXT: }

// CHECK:      struct PrivatelyInheritsPrivatelyInheritsProtectedCopy {
// CHECK-NEXT:   init()
// CHECK-NEXT: }

// CHECK:      struct InheritsProtectedMove : ~Copyable {
// CHECK-NEXT:   init()
// CHECK-NEXT:   func getFromBase() -> CInt
// CHECK-NEXT:   mutating func setFromBase(_ x: CInt)
// CHECK-NEXT: }

// CHECK:      struct ProtectedCopyWithMove : ~Copyable {
// CHECK-NEXT:   var fromBase: CInt
// CHECK-NEXT:   init()
// CHECK-NEXT: }

// CHECK:      struct InheritsProtectedCopyWithMove {
// CHECK-NEXT:   init()
// CHECK-NEXT:   var fromBase: CInt
// CHECK-NEXT:   func getFromBase() -> CInt
// CHECK-NEXT:   mutating func setFromBase(_ x: CInt)
// CHECK-NEXT: }
