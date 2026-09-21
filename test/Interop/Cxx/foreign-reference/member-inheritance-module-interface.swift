// RUN: %target-swift-ide-test -source-filename=x -print-module -print-implicit-attrs \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -module-to-print=MemberInheritance -I %S/Inputs  \
// RUN: | %FileCheck %s

// CHECK: class ImmortalBase {
// CHECK:  func get42() -> CInt
// CHECK:  func getOverridden42() -> CInt
// CHECK: }
// CHECK: class Immortal : ImmortalBase {
// CHECK:  override func getOverridden42() -> CInt
// CHECK: }

// CHECK: class Immortal2 {
// CHECK-NEXT: final func virtualMethod(_: HasDestructor)
// CHECK-NEXT: final func swiftVirtualRename()
// CHECK: }

// CHECK: class A1 {
// CHECK:  final func virtualMethod() -> CInt
// CHECK:  final func swiftFooRename() -> CInt
// CHECK:  final func swiftBarRename() -> CInt
// CHECK:  final func swiftParamsRename(a1 i: CInt) -> CInt
// CHECK: }

// CHECK: class B1 : A1 {
// CHECK:  final override func virtualMethod() -> CInt
// CHECK:  final override func swiftFooRename() -> CInt
// CHECK:  final override func swiftBarRename() -> CInt
// CHECK:  final override func swiftParamsRename(a1 i: CInt) -> CInt
// CHECK: }

// CHECK: class B2 : A1 {
// CHECK:   final override func virtualMethod() -> CInt
// CHECK:   final override func swiftFooRename() -> CInt
// CHECK:   final override func swiftBarRename() -> CInt
// CHECK: }

// CHECK: class C1 : B1 {
// CHECK:  final override func swiftFooRename() -> CInt
// CHECK:  final override func swiftBarRename() -> CInt
// CHECK:  final override func swiftParamsRename(a1 i: CInt) -> CInt
// CHECK: }

// CHECK: class C2 : B1 {
// CHECK:  final override func virtualMethod() -> CInt
// CHECK:  final override func swiftFooRename() -> CInt
// CHECK:  final override func swiftBarRename() -> CInt
// CHECK:  final override func swiftParamsRename(a1 i: CInt) -> CInt
// CHECK: }

// CHECK: class A2 {
// CHECK:  final func swiftVirtualMethod() -> CInt
// CHECK:  final func swiftFooRename() -> CInt
// CHECK:  final func A2BarRename() -> CInt
// CHECK:  final func swiftParamsRename(a2 i: CInt) -> CInt
// CHECK: }

// CHECK: class D1 {
// CHECK:  final func virtualMethod() -> CInt
// CHECK:  final func swiftFooRename() -> CInt
// CHECK:  final func swiftBarRename() -> CInt
// CHECK:  final func swiftParamsRename(a1 i: CInt) -> CInt
// CHECK:  final func swiftVirtualMethod() -> CInt
// CHECK:  final func A2BarRename() -> CInt
// CHECK:  final func swiftParamsRename(a2 i: CInt) -> CInt
// CHECK: }

// CHECK: class D2 {
// CHECK:  final func swiftFooRename() -> CInt
// CHECK:  @available(*, unavailable, message: "overrides{{.*}}")
// CHECK:  final func virtualMethod() -> CInt
// CHECK:  @available(*, unavailable, message: "overrides{{.*}}")
// CHECK:  final func swiftBarRename() -> CInt
// CHECK:  @available(*, unavailable, message: "overrides{{.*}}")
// CHECK:  final func swiftParamsRename(a1 i: CInt) -> CInt
// CHECK:  @available(*, unavailable, message: "overrides{{.*}}")
// CHECK:  final func swiftVirtualMethod() -> CInt
// CHECK:  @available(*, unavailable, message: "overrides{{.*}}")
// CHECK:  final func A2BarRename() -> CInt
// CHECK:  @available(*, unavailable, message: "overrides{{.*}}")
// CHECK:  final func swiftParamsRename(a2 i: CInt) -> CInt
// CHECK: }

// CHECK: class D3 {
// CHECK:  final func virtualMethod() -> CInt
// CHECK:  final func swiftFooRename() -> CInt
// CHECK:  final func swiftBarRename() -> CInt
// CHECK:  final func swiftParamsRename(a1 i: CInt) -> CInt
// CHECK: }

// CHECK: class D4 {
// CHECK:  final func swiftFooRename() -> CInt
// CHECK:  final func swiftBarRename() -> CInt
// CHECK:  final func swiftParamsRename(a1 i: CInt) -> CInt
// CHECK:  final func virtualMethod() -> CInt
// CHECK: }

// CHECK: struct ValueType {
// CHECK:   func virtualMethod() -> CInt
// CHECK:   func swiftRenameMethodBase() -> CInt
// CHECK:   func renameMethodDerived() -> CInt
// CHECK:   @available(*, unavailable, message: "virtual function is not available in Swift because it is pure")
// CHECK:   func pureVirtualMethod() -> CInt
// CHECK:   @available(*, unavailable, message: "virtual function is not available in Swift because it is pure")
// CHECK:   func swiftPureRenameBase() -> CInt
// CHECK:   @available(*, unavailable, message: "virtual function is not available in Swift because it is pure")
// CHECK:   func pureRenameDerived() -> CInt
// CHECK: }

// CHECK: class DerivedFRTValueType {
// CHECK:   final func virtualMethod() -> CInt
// CHECK:   final func swiftRenameMethodBase() -> CInt
// CHECK:   final func renameMethodDerived() -> CInt
// CHECK:   final func pureVirtualMethod() -> CInt
// CHECK:   final func swiftPureRenameBase() -> CInt
// CHECK:   final func pureRenameDerived() -> CInt
// CHECK: }

// CHECK: class EmptyDerivedFRTValueType {
// CHECK:   func virtualMethod() -> CInt
// CHECK:   func swiftRenameMethodBase() -> CInt
// CHECK:   func renameMethodDerived() -> CInt
// CHECK:   @available(*, unavailable, message: "virtual function is not available in Swift because it is pure")
// CHECK:   func pureVirtualMethod() -> CInt
// CHECK:   @available(*, unavailable, message: "virtual function is not available in Swift because it is pure")
// CHECK:   func swiftPureRenameBase() -> CInt
// CHECK:   @available(*, unavailable, message: "virtual function is not available in Swift because it is pure")
// CHECK:   func pureRenameDerived() -> CInt
// CHECK: }

// CHECK: struct DerivedValueType {
// CHECK:   func virtualMethod() -> CInt
// CHECK:   func swiftRenameMethodBase() -> CInt
// CHECK:   func renameMethodDerived() -> CInt
// CHECK:   func pureVirtualMethod() -> CInt
// CHECK:   func swiftPureRenameBase() -> CInt
// CHECK:   func pureRenameDerived() -> CInt
// CHECK: }

// CHECK: class AbstractFRT {
// CHECK:   final func pureVirtualMethod() -> CInt
// CHECK:   final func swiftPureRenameBase() -> CInt
// CHECK:   final func pureRenameDerived() -> CInt
// CHECK: }

// CHECK: class DerivedAbstractFRT : AbstractFRT {
// CHECK:   final override func pureVirtualMethod() -> CInt
// CHECK:   final override func swiftPureRenameBase() -> CInt
// CHECK:   final override func pureRenameDerived() -> CInt
// CHECK: }

// CHECK: class EmptyDerivedAbstractFRT : AbstractFRT {
// CHECK: }

// CHECK: class BaseWithInitMethod {
// CHECK:   func `init`() -> CInt
// CHECK: }

// CHECK: class DerivedWithInitMethod : BaseWithInitMethod {
// CHECK:   func `init`() -> CInt
// CHECK: }

// CHECK: class RenamedBaseWithInitMethod {
// CHECK:   func method() -> CInt
// CHECK: }

// CHECK: class RenamedDerivedWithInitMethod : RenamedBaseWithInitMethod {
// CHECK: }
