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
// CHECK:  final func virtualMethod() -> Int32
// CHECK:  final func swiftFooRename() -> Int32
// CHECK:  final func swiftBarRename() -> Int32
// CHECK:  final func swiftParamsRename(a1 i: Int32) -> Int32
// CHECK: }

// CHECK: class B2 {
// CHECK:   final func virtualMethod() -> Int32
// CHECK:   final func swiftFooRename() -> Int32
// CHECK:   final func swiftBarRename() -> Int32
// CHECK:   final func swiftParamsRename(a1 i: Int32) -> Int32
// CHECK: }

// CHECK: class C1 {
// CHECK:  final func swiftFooRename() -> Int32
// CHECK:  final func swiftBarRename() -> Int32
// CHECK:  final func swiftParamsRename(a1 i: Int32) -> Int32
// CHECK:  final func virtualMethod() -> Int32
// CHECK: }

// CHECK: class C2 {
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
// CHECK:  final func swiftFooRename() -> Int32
// CHECK:  @available(*, unavailable, message: "overrides{{.*}}")
// CHECK:  final func virtualMethod() -> Int32
// CHECK:  @available(*, unavailable, message: "overrides{{.*}}")
// CHECK:  final func swiftBarRename() -> Int32
// CHECK:  @available(*, unavailable, message: "overrides{{.*}}")
// CHECK:  final func swiftParamsRename(a1 i: Int32) -> Int32
// CHECK:  @available(*, unavailable, message: "overrides{{.*}}")
// CHECK:  final func swiftVirtualMethod() -> Int32
// CHECK:  @available(*, unavailable, message: "overrides{{.*}}")
// CHECK:  final func A2BarRename() -> Int32
// CHECK:  @available(*, unavailable, message: "overrides{{.*}}")
// CHECK:  final func swiftParamsRename(a2 i: Int32) -> Int32
// CHECK: }

// CHECK: struct D3 {
// CHECK:  final func virtualMethod() -> Int32
// CHECK:  final func swiftFooRename() -> Int32
// CHECK:  final func swiftBarRename() -> Int32
// CHECK:  final func swiftParamsRename(a1 i: Int32) -> Int32
// CHECK: }

// CHECK: struct D4 {
// CHECK:  final func swiftFooRename() -> Int32
// CHECK:  final func swiftBarRename() -> Int32
// CHECK:  final func swiftParamsRename(a1 i: Int32) -> Int32
// CHECK:  final func virtualMethod() -> Int32
// CHECK: }

// CHECK: struct ValueType {
// CHECK:   func virtualMethod() -> Int32
// CHECK:   func swiftRenameMethodBase() -> Int32
// CHECK:   func renameMethodDerived() -> Int32
// CHECK:   @available(*, unavailable, message: "virtual function is not available in Swift because it is pure")
// CHECK:   func pureVirtualMethod() -> Int32
// CHECK:   @available(*, unavailable, message: "virtual function is not available in Swift because it is pure")
// CHECK:   func swiftPureRenameBase() -> Int32
// CHECK:   @available(*, unavailable, message: "virtual function is not available in Swift because it is pure")
// CHECK:   func pureRenameDerived() -> Int32
// CHECK: }

// CHECK: class DerivedFRTValueType {
// CHECK:   final func virtualMethod() -> Int32
// CHECK:   final func swiftRenameMethodBase() -> Int32
// CHECK:   final func renameMethodDerived() -> Int32
// CHECK:   final func pureVirtualMethod() -> Int32
// CHECK:   final func swiftPureRenameBase() -> Int32
// CHECK:   final func pureRenameDerived() -> Int32
// CHECK: }

// CHECK: class EmptyDerivedFRTValueType {
// CHECK:   func virtualMethod() -> Int32
// CHECK:   func swiftRenameMethodBase() -> Int32
// CHECK:   func renameMethodDerived() -> Int32
// CHECK:   @available(*, unavailable, message: "virtual function is not available in Swift because it is pure")
// CHECK:   func pureVirtualMethod() -> Int32
// CHECK:   @available(*, unavailable, message: "virtual function is not available in Swift because it is pure")
// CHECK:   func swiftPureRenameBase() -> Int32
// CHECK:   @available(*, unavailable, message: "virtual function is not available in Swift because it is pure")
// CHECK:   func pureRenameDerived() -> Int32
// CHECK: }

// CHECK: struct DerivedValueType {
// CHECK:   func virtualMethod() -> Int32
// CHECK:   func swiftRenameMethodBase() -> Int32
// CHECK:   func renameMethodDerived() -> Int32
// CHECK:   func pureVirtualMethod() -> Int32
// CHECK:   func swiftPureRenameBase() -> Int32
// CHECK:   func pureRenameDerived() -> Int32
// CHECK: }

// CHECK: class AbstractFRT {
// CHECK:   final func pureVirtualMethod() -> Int32
// CHECK:   final func swiftPureRenameBase() -> Int32
// CHECK:   final func pureRenameDerived() -> Int32
// CHECK: }

// CHECK: class DerivedAbstractFRT {
// CHECK:   init()
// CHECK:   final func pureVirtualMethod() -> Int32
// CHECK:   final func swiftPureRenameBase() -> Int32
// CHECK:   final func pureRenameDerived() -> Int32
// CHECK: }

// CHECK: class EmptyDerivedAbstractFRT {
// CHECK:   final func pureVirtualMethod() -> Int32
// CHECK:   final func swiftPureRenameBase() -> Int32
// CHECK:   final func pureRenameDerived() -> Int32
// CHECK: }
