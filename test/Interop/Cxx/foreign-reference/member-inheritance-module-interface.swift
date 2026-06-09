// RUN: %target-swift-ide-test -print-module -cxx-interoperability-mode=swift-5.9 -print-implicit-attrs -module-to-print=MemberInheritance -I %S/Inputs -source-filename=x | %FileCheck --check-prefixes=CHECK,CHECK-OFF %s
// RUN: %target-swift-ide-test -print-module -cxx-interoperability-mode=swift-6 -print-implicit-attrs -module-to-print=MemberInheritance -I %S/Inputs -source-filename=x | %FileCheck --check-prefixes=CHECK,CHECK-OFF %s
// RUN: %target-swift-ide-test -print-module -cxx-interoperability-mode=upcoming-swift -print-implicit-attrs -module-to-print=MemberInheritance -I %S/Inputs -source-filename=x | %FileCheck --check-prefixes=CHECK,CHECK-OFF %s
// RUN: %target-swift-ide-test -print-module -cxx-interoperability-mode=upcoming-swift -enable-experimental-feature ForeignReferenceTypeInheritance -print-implicit-attrs -module-to-print=MemberInheritance -I %S/Inputs -source-filename=x | %FileCheck --check-prefixes=CHECK,CHECK-ON %s

// REQUIRES: swift_feature_ForeignReferenceTypeInheritance

// CHECK: class ImmortalBase {
// CHECK:  func get42() -> Int32
// CHECK:  func getOverridden42() -> Int32
// CHECK: }
// CHECK-OFF: class Immortal {
// CHECK-OFF:  func getOverridden42() -> Int32
// CHECK-OFF:  func get42() -> Int32
// CHECK-OFF: }
// CHECK-ON: class Immortal : ImmortalBase {
// CHECK-ON:  override func getOverridden42() -> Int32
// CHECK-ON: }

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

// CHECK-OFF: class B1 {
// CHECK-OFF:  final func virtualMethod() -> Int32
// CHECK-OFF:  final func swiftFooRename() -> Int32
// CHECK-OFF:  final func swiftBarRename() -> Int32
// CHECK-OFF:  final func swiftParamsRename(a1 i: Int32) -> Int32
// CHECK-OFF: }
// CHECK-ON: class B1 : A1 {
// CHECK-ON:  final override func virtualMethod() -> Int32
// CHECK-ON:  final override func swiftFooRename() -> Int32
// CHECK-ON:  final override func swiftBarRename() -> Int32
// CHECK-ON:  final override func swiftParamsRename(a1 i: Int32) -> Int32
// CHECK-ON: }

// CHECK-OFF: class B2 {
// CHECK-OFF:   final func virtualMethod() -> Int32
// CHECK-OFF:   final func swiftFooRename() -> Int32
// CHECK-OFF:   final func swiftBarRename() -> Int32
// CHECK-OFF:   final func swiftParamsRename(a1 i: Int32) -> Int32
// CHECK-OFF: }
// CHECK-ON: class B2 : A1 {
// CHECK-ON:   final override func virtualMethod() -> Int32
// CHECK-ON:   final override func swiftFooRename() -> Int32
// CHECK-ON:   final override func swiftBarRename() -> Int32
// CHECK-ON: }

// CHECK-OFF: class C1 {
// CHECK-OFF:  final func swiftFooRename() -> Int32
// CHECK-OFF:  final func swiftBarRename() -> Int32
// CHECK-OFF:  final func swiftParamsRename(a1 i: Int32) -> Int32
// CHECK-OFF:  final func virtualMethod() -> Int32
// CHECK-OFF: }
// CHECK-ON: class C1 : B1 {
// CHECK-ON:  final override func swiftFooRename() -> Int32
// CHECK-ON:  final override func swiftBarRename() -> Int32
// CHECK-ON:  final override func swiftParamsRename(a1 i: Int32) -> Int32
// CHECK-ON: }

// CHECK-OFF: class C2 {
// CHECK-OFF:  final func virtualMethod() -> Int32
// CHECK-OFF:  final func swiftFooRename() -> Int32
// CHECK-OFF:  final func swiftBarRename() -> Int32
// CHECK-OFF:  final func swiftParamsRename(a1 i: Int32) -> Int32
// CHECK-OFF: }
// CHECK-ON: class C2 : B1 {
// CHECK-ON:  final override func virtualMethod() -> Int32
// CHECK-ON:  final override func swiftFooRename() -> Int32
// CHECK-ON:  final override func swiftBarRename() -> Int32
// CHECK-ON:  final override func swiftParamsRename(a1 i: Int32) -> Int32
// CHECK-ON: }

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

// CHECK: class D3 {
// CHECK:  final func virtualMethod() -> Int32
// CHECK:  final func swiftFooRename() -> Int32
// CHECK:  final func swiftBarRename() -> Int32
// CHECK:  final func swiftParamsRename(a1 i: Int32) -> Int32
// CHECK: }

// CHECK: class D4 {
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

// CHECK-OFF: class DerivedAbstractFRT {
// CHECK-OFF:   init()
// CHECK-OFF:   final func pureVirtualMethod() -> Int32
// CHECK-OFF:   final func swiftPureRenameBase() -> Int32
// CHECK-OFF:   final func pureRenameDerived() -> Int32
// CHECK-OFF: }
// CHECK-ON: class DerivedAbstractFRT : AbstractFRT {
// CHECK-ON:   final override func pureVirtualMethod() -> Int32
// CHECK-ON:   final override func swiftPureRenameBase() -> Int32
// CHECK-ON:   final override func pureRenameDerived() -> Int32
// CHECK-ON: }

// CHECK-OFF: class EmptyDerivedAbstractFRT {
// CHECK-OFF:   final func pureVirtualMethod() -> Int32
// CHECK-OFF:   final func swiftPureRenameBase() -> Int32
// CHECK-OFF:   final func pureRenameDerived() -> Int32
// CHECK-OFF: }
// CHECK-ON: class EmptyDerivedAbstractFRT : AbstractFRT {
// CHECK-ON: }
