// RUN: %target-swift-ide-test -print-module -module-to-print=Upcast -I %S/Inputs -I %swift_src_root/lib/ClangImporter/SwiftBridging -source-filename=x -cxx-interoperability-mode=upcoming-swift -enable-experimental-feature ForeignReferenceTypeInheritance -print-implicit-attrs | %FileCheck %s

// REQUIRES: swift_feature_ForeignReferenceTypeInheritance

// CHECK: class Base {
// CHECK:   var baseValue: CInt
// CHECK:   func getBaseValue() -> CInt
// CHECK:   func setBaseValue(_ v: CInt)
// CHECK:   class func create() -> Base
// CHECK: }

// CHECK: class Derived : Base {
// CHECK:   var derivedValue: CInt
// CHECK:   func getDerivedValue() -> CInt
// CHECK:   override class func create() -> Derived
// CHECK: }

// CHECK: class LeafDerived : Derived {
// CHECK:   var leafValue: CInt
// CHECK:   func getLeafValue() -> CInt
// CHECK:   override class func create() -> LeafDerived
// CHECK: }

// CHECK: class Unrelated {
// CHECK:   var unrelatedValue: CInt
// CHECK:   class func create() -> Unrelated
// CHECK: }

// Virtual base: no Swift superclass relationship.
// CHECK: class VirtualDerived {

// CHECK: class RefCountedBase {
// CHECK:   var baseField: CInt
// CHECK:   func getBaseValue() -> CInt
// CHECK:   class func create() -> RefCountedBase
// CHECK: }

// CHECK: class RefCountedDerived : RefCountedBase {
// CHECK:   var derivedField: CInt
// CHECK:   func getDerivedField() -> CInt
// CHECK:   override class func create() -> RefCountedDerived
// CHECK: }

// CHECK: class OverridesLifetimeOps {

// CHECK: class OverridesLifetimeOpsDerived {

// CHECK: class ReannotatedRefCountedDerived {

// CHECK: class DerivedFromEmptyAndBase : Base {
// CHECK:   var extraValue: CInt
// CHECK:   func getExtraValue() -> CInt
// CHECK:   override class func create() -> DerivedFromEmptyAndBase
// CHECK: }

// CHECK: class CRTPBase<CRTPDerived> {
// CHECK:   var crtpBaseValue: CInt
// CHECK:   func derivedSelf() -> CRTPDerived!
// CHECK: }

// CHECK: class CRTPDerived : CRTPBase<CRTPDerived> {
// CHECK:   var crtpDerivedValue: CInt
// CHECK:   class func create() -> CRTPDerived
// CHECK: }

// CHECK: typealias CRTPBaseOfDerived = CRTPBase<CRTPDerived>
