// RUN: %target-swift-ide-test -source-filename=x -print-module -print-implicit-attrs \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -module-to-print=MemberInheritance -I %S/Inputs  \
// RUN: | %FileCheck --check-prefixes=CHECK,CHECK-OFF %s
// RUN: %target-swift-ide-test -source-filename=x -print-module -print-implicit-attrs \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -module-to-print=MemberInheritance -I %S/Inputs  \
// RUN:   -enable-experimental-feature ForeignReferenceTypeInheritance \
// RUN: | %FileCheck --check-prefixes=CHECK,CHECK-ON %s

// REQUIRES: swift_feature_ForeignReferenceTypeInheritance

// CHECK: class ImmortalBase {
// CHECK:  func get42() -> CInt
// CHECK:  func getOverridden42() -> CInt
// CHECK: }
// CHECK-OFF: class Immortal {
// CHECK-OFF:  func getOverridden42() -> CInt
// CHECK-OFF:  func get42() -> CInt
// CHECK-OFF: }
// CHECK-ON: class Immortal : ImmortalBase {
// CHECK-ON:  override func getOverridden42() -> CInt
// CHECK-ON: }

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

// CHECK-OFF: class B1 {
// CHECK-OFF:  final func virtualMethod() -> CInt
// CHECK-OFF:  final func swiftFooRename() -> CInt
// CHECK-OFF:  final func swiftBarRename() -> CInt
// CHECK-OFF:  final func swiftParamsRename(a1 i: CInt) -> CInt
// CHECK-OFF: }
// CHECK-ON: class B1 : A1 {
// CHECK-ON:  final override func virtualMethod() -> CInt
// CHECK-ON:  final override func swiftFooRename() -> CInt
// CHECK-ON:  final override func swiftBarRename() -> CInt
// CHECK-ON:  final override func swiftParamsRename(a1 i: CInt) -> CInt
// CHECK-ON: }

// CHECK-OFF: class B2 {
// CHECK-OFF:   final func virtualMethod() -> CInt
// CHECK-OFF:   final func swiftFooRename() -> CInt
// CHECK-OFF:   final func swiftBarRename() -> CInt
// CHECK-OFF:   final func swiftParamsRename(a1 i: CInt) -> CInt
// CHECK-OFF: }
// CHECK-ON: class B2 : A1 {
// CHECK-ON:   final override func virtualMethod() -> CInt
// CHECK-ON:   final override func swiftFooRename() -> CInt
// CHECK-ON:   final override func swiftBarRename() -> CInt
// CHECK-ON: }

// CHECK-OFF: class C1 {
// CHECK-OFF:  final func swiftFooRename() -> CInt
// CHECK-OFF:  final func swiftBarRename() -> CInt
// CHECK-OFF:  final func swiftParamsRename(a1 i: CInt) -> CInt
// CHECK-OFF:  final func virtualMethod() -> CInt
// CHECK-OFF: }
// CHECK-ON: class C1 : B1 {
// CHECK-ON:  final override func swiftFooRename() -> CInt
// CHECK-ON:  final override func swiftBarRename() -> CInt
// CHECK-ON:  final override func swiftParamsRename(a1 i: CInt) -> CInt
// CHECK-ON: }

// CHECK-OFF: class C2 {
// CHECK-OFF:  final func virtualMethod() -> CInt
// CHECK-OFF:  final func swiftFooRename() -> CInt
// CHECK-OFF:  final func swiftBarRename() -> CInt
// CHECK-OFF:  final func swiftParamsRename(a1 i: CInt) -> CInt
// CHECK-OFF: }
// CHECK-ON: class C2 : B1 {
// CHECK-ON:  final override func virtualMethod() -> CInt
// CHECK-ON:  final override func swiftFooRename() -> CInt
// CHECK-ON:  final override func swiftBarRename() -> CInt
// CHECK-ON:  final override func swiftParamsRename(a1 i: CInt) -> CInt
// CHECK-ON: }

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

// CHECK-OFF: class DerivedAbstractFRT {
// CHECK-OFF:   init()
// CHECK-OFF:   final func pureVirtualMethod() -> CInt
// CHECK-OFF:   final func swiftPureRenameBase() -> CInt
// CHECK-OFF:   final func pureRenameDerived() -> CInt
// CHECK-OFF: }
// CHECK-ON: class DerivedAbstractFRT : AbstractFRT {
// CHECK-ON:   final override func pureVirtualMethod() -> CInt
// CHECK-ON:   final override func swiftPureRenameBase() -> CInt
// CHECK-ON:   final override func pureRenameDerived() -> CInt
// CHECK-ON: }

// CHECK-OFF: class EmptyDerivedAbstractFRT {
// CHECK-OFF:   final func pureVirtualMethod() -> CInt
// CHECK-OFF:   final func swiftPureRenameBase() -> CInt
// CHECK-OFF:   final func pureRenameDerived() -> CInt
// CHECK-OFF: }
// CHECK-ON: class EmptyDerivedAbstractFRT : AbstractFRT {
// CHECK-ON: }

// CHECK: class BaseWithInitMethod {
// CHECK:   func `init`() -> CInt
// CHECK: }

// CHECK-OFF: class DerivedWithInitMethod {
// CHECK-OFF:   func `init`() -> CInt
// CHECK-OFF: }
// CHECK-ON: class DerivedWithInitMethod : BaseWithInitMethod {
// CHECK-ON:   func `init`() -> CInt
// CHECK-ON: }

// CHECK: class RenamedBaseWithInitMethod {
// CHECK:   func method() -> CInt
// CHECK: }

// CHECK-OFF: class RenamedDerivedWithInitMethod {
// CHECK-OFF:   func method() -> CInt
// CHECK-OFF: }
// CHECK-ON: class RenamedDerivedWithInitMethod : RenamedBaseWithInitMethod {
// CHECK-ON-NOT:   func method() -> CInt
// CHECK-ON: }
