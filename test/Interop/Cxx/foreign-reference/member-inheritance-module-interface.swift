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

// CHECK: class A1 {
// CHECK:  final func virtualMethod() -> Int32
// CHECK:  final func swiftFooRename() -> Int32
// CHECK:  final func swiftBarRename() -> Int32
// CHECK:  final func swiftParamsRename(a1 i: Int32) -> Int32
// CHECK: }

// CHECK: class B1 {
// CHECK:  @available(*, unavailable, message: "ignoring{{.*}}")
// CHECK:  final func swiftVirtualMethod() -> Int32
// CHECK:  @available(*, unavailable, message: "ignoring{{.*}}")
// CHECK:  final func B1BarRename() -> Int32
// CHECK:  @available(*, unavailable, message: "ignoring{{.*}}")
// CHECK:  final func swiftParamsRename(b1 i: Int32) -> Int32
// CHECK:  final func virtualMethod() -> Int32
// CHECK:  final func swiftFooRename() -> Int32
// CHECK:  final func swiftBarRename() -> Int32
// CHECK:  final func swiftParamsRename(a1 i: Int32) -> Int32
// CHECK: }

// CHECK: class B2 {
// CHECK:    @available(*, unavailable, message: "ignoring{{.*}}")
// CHECK:   final func B2BarRename() -> Int32
// CHECK:   final func virtualMethod() -> Int32
// CHECK:   final func swiftFooRename() -> Int32
// CHECK:   final func swiftBarRename() -> Int32
// CHECK:   final func swiftParamsRename(a1 i: Int32) -> Int32
// CHECK: }

// CHECK: class C1 {
// CHECK:  final func swiftFooRename() -> Int32
// CHECK:  final func swiftBarRename() -> Int32
// CHECK:  @available(*, unavailable, message: "ignoring{{.*}}")
// CHECK:  final func swiftVirtualMethod() -> Int32
// CHECK:  @available(*, unavailable, message: "ignoring{{.*}}")
// CHECK:  final func B1BarRename() -> Int32
// CHECK:  @available(*, unavailable, message: "ignoring{{.*}}")
// CHECK:  final func swiftParamsRename(b1 i: Int32) -> Int32
// CHECK:  final func virtualMethod() -> Int32
// CHECK:  final func swiftParamsRename(a1 i: Int32) -> Int32
// CHECK: }

// CHECK: class C2 {
// CHECK:  @available(*, unavailable, message: "ignoring{{.*}}")
// CHECK:  final func swiftVirtualMethod() -> Int32
// CHECK:  @available(*, unavailable, message: "ignoring{{.*}}")
// CHECK:  final func C2FooRename() -> Int32
// CHECK:  @available(*, unavailable, message: "ignoring{{.*}}")
// CHECK:  final func B1BarRename() -> Int32
// CHECK:  @available(*, unavailable, message: "ignoring{{.*}}")
// CHECK:  final func swiftParamsRename(b1 i: Int32) -> Int32
// CHECK:  final func virtualMethod() -> Int32
// CHECK:  final func swiftFooRename() -> Int32
// CHECK:  final func swiftBarRename() -> Int32
// CHECK:  final func swiftParamsRename(a1 i: Int32) -> Int32
// CHECK: }

// CHECK: class A2 {
// CHECK:  final func swiftVirtualMethod() -> Int32
// CHECK:  final func swiftFooRename() -> Int32
// CHECK:  final func A2BarRename() -> Int32
// CHECK:  final func swiftParamsRename(a2 i: Int32) -> Int32
// CHECK: }

// CHECK: class D1 {
// CHECK:  final func virtualMethod() -> Int32
// CHECK:  final func swiftFooRename() -> Int32
// CHECK:  final func swiftBarRename() -> Int32
// CHECK:  final func swiftParamsRename(a1 i: Int32) -> Int32
// CHECK:  final func swiftVirtualMethod() -> Int32
// CHECK:  final func A2BarRename() -> Int32
// CHECK:  final func swiftParamsRename(a2 i: Int32) -> Int32
// CHECK: }

// CHECK: class D2 {
// CHECK:  @available(*, unavailable, message: "ignoring{{.*}}")
// CHECK:  final func swiftVirtualMethod() -> Int32
// CHECK:  @available(*, unavailable, message: "ignoring{{.*}}")
// CHECK:  final func B1BarRename() -> Int32
// CHECK:  @available(*, unavailable, message: "ignoring{{.*}}")
// CHECK:  final func swiftParamsRename(b1 i: Int32) -> Int32
// CHECK:  @available(*, unavailable, message: "overrides{{.*}}")
// CHECK:  final func virtualMethod() -> Int32
// CHECK:  final func swiftFooRename() -> Int32
// CHECK:  @available(*, unavailable, message: "overrides{{.*}}")
// CHECK:  final func swiftBarRename() -> Int32
// CHECK:  @available(*, unavailable, message: "overrides{{.*}}")
// CHECK:  final func swiftParamsRename(a1 i: Int32) -> Int32
// CHECK:  @available(*, unavailable, message: "overrides{{.*}}")
// CHECK:  final func A2BarRename() -> Int32
// CHECK:  @available(*, unavailable, message: "overrides{{.*}}")
// CHECK:  final func swiftParamsRename(a2 i: Int32) -> Int32
// CHECK: }

// CHECK: struct D3 {
// CHECK:  @available(*, unavailable, message: "ignoring{{.*}}")
// CHECK:  final func swiftVirtualMethod() -> Int32
// CHECK:  @available(*, unavailable, message: "ignoring{{.*}}")
// CHECK:  final func B1BarRename() -> Int32
// CHECK:  @available(*, unavailable, message: "ignoring{{.*}}")
// CHECK:  final func swiftParamsRename(b1 i: Int32) -> Int32
// CHECK:  final func virtualMethod() -> Int32
// CHECK:  final func swiftFooRename() -> Int32
// CHECK:  final func swiftBarRename() -> Int32
// CHECK:  final func swiftParamsRename(a1 i: Int32) -> Int32
// CHECK: }

// CHECK: struct D4 {
// CHECK:  final func swiftFooRename() -> Int32
// CHECK:  final func swiftBarRename() -> Int32
// CHECK:  @available(*, unavailable, message: "ignoring{{.*}}")
// CHECK:  final func swiftVirtualMethod() -> Int32
// CHECK:  @available(*, unavailable, message: "ignoring{{.*}}")
// CHECK:  final func B1BarRename() -> Int32
// CHECK:  @available(*, unavailable, message: "ignoring{{.*}}")
// CHECK:  final func swiftParamsRename(b1 i: Int32) -> Int32
// CHECK:  final func virtualMethod() -> Int32
// CHECK:  final func swiftParamsRename(a1 i: Int32) -> Int32
// CHECK: }
