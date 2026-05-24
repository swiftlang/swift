// RUN: %target-swift-ide-test -print-module -module-to-print=Upcast -I %S/Inputs -I %swift_src_root/lib/ClangImporter/SwiftBridging -source-filename=x -cxx-interoperability-mode=upcoming-swift -print-implicit-attrs | %FileCheck %s

// CHECK: class Base {
// CHECK:   var baseValue: Int32
// CHECK:   func getBaseValue() -> Int32
// CHECK:   func setBaseValue(_ v: Int32)
// CHECK:   class func create() -> Base
// CHECK: }

// CHECK: class Derived : Base {
// CHECK:   var derivedValue: Int32
// CHECK:   func getDerivedValue() -> Int32
// CHECK:   override class func create() -> Derived
// CHECK: }

// CHECK: class LeafDerived : Derived {
// CHECK:   var leafValue: Int32
// CHECK:   func getLeafValue() -> Int32
// CHECK:   override class func create() -> LeafDerived
// CHECK: }

// CHECK: class Unrelated {
// CHECK:   var unrelatedValue: Int32
// CHECK:   class func create() -> Unrelated
// CHECK: }

// Virtual base: no Swift superclass relationship.
// CHECK: class VirtualDerived {

// CHECK: class RefCountedBase {
// CHECK:   var baseField: Int32
// CHECK:   func getBaseValue() -> Int32
// CHECK:   class func create() -> RefCountedBase
// CHECK: }

// CHECK: class RefCountedDerived : RefCountedBase {
// CHECK:   var derivedField: Int32
// CHECK:   func getDerivedField() -> Int32
// CHECK:   override class func create() -> RefCountedDerived
// CHECK: }

// CHECK: class OverridesLifetimeOps {

// CHECK: class OverridesLifetimeOpsDerived : OverridesLifetimeOps {

// CHECK: class DerivedFromEmptyAndBase : Base {
// CHECK:   var extraValue: Int32
// CHECK:   func getExtraValue() -> Int32
// CHECK:   override class func create() -> DerivedFromEmptyAndBase
// CHECK: }

// CHECK: class CRTPBase<CRTPDerived> {
// CHECK:   var crtpBaseValue: Int32
// CHECK:   func derivedSelf() -> CRTPDerived!
// CHECK: }

// CHECK: class CRTPDerived : CRTPBase<CRTPDerived> {
// CHECK:   var crtpDerivedValue: Int32
// CHECK:   class func create() -> CRTPDerived
// CHECK: }

// CHECK: typealias CRTPBaseOfDerived = CRTPBase<CRTPDerived>
