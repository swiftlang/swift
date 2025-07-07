// RUN: %target-swift-ide-test -print-module -cxx-interoperability-mode=swift-5.9 -print-implicit-attrs -module-to-print=MemberInheritance -I %S/Inputs -source-filename=x | %FileCheck %s
// RUN: %target-swift-ide-test -print-module -cxx-interoperability-mode=swift-6 -print-implicit-attrs -module-to-print=MemberInheritance -I %S/Inputs -source-filename=x | %FileCheck %s
// RUN: %target-swift-ide-test -print-module -cxx-interoperability-mode=upcoming-swift -print-implicit-attrs -module-to-print=MemberInheritance -I %S/Inputs -source-filename=x | %FileCheck %s

// CHECK: class ImmortalBase {
// CHECK:  func get42() -> Int32
// CHECK:  func getOverridden42() -> Int32
// CHECK: }
// CHECK: class Immortal {
// CHECK:  func getOverridden42() -> Int32
// CHECK:  func get42() -> Int32
// CHECK: }

// CHECK: class Immortal2 {
// CHECK-NEXT: final func virtualMethod(_: HasDestructor)
// CHECK-NEXT: final func swiftVirtualRename()
// CHECK: }
