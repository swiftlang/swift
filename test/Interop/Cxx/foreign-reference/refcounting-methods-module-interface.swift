// RUN: %target-swift-ide-test -print-module -cxx-interoperability-mode=upcoming-swift -module-to-print=RefCountingMethods -I %S/Inputs -source-filename=x | %FileCheck %s
// RUN: %target-swift-ide-test -print-module -cxx-interoperability-mode=upcoming-swift -module-to-print=RefCountingMethods -I %S/Inputs -source-filename=x -print-implicit-attrs | %FileCheck %s --check-prefix=UNSAFE

// CHECK: class RefCountedBox {
// CHECK:   func doRetain()
// CHECK:   func doRelease()
// CHECK: }
// CHECK: class DerivedRefCountedBox : RefCountedBox {
// CHECK: }

// CHECK: class DerivedHasRelease {
// CHECK:   func doRetainInBase()
// CHECK:   func doRelease()
// CHECK: }

// CHECK: class TemplatedDerivedHasRelease<CFloat> {
// CHECK:   var value: CFloat
// CHECK:   func doReleaseTemplated()
// CHECK:   func doRetainInBase()
// CHECK: }
// CHECK: class TemplatedDerivedHasRelease<CInt> {
// CHECK:   var value: CInt
// CHECK:   func doReleaseTemplated()
// CHECK:   func doRetainInBase()
// CHECK: }

// CHECK: class CRTPDerived : CRTPBase<CRTPDerived> {
// CHECK:   var value: CInt
// CHECK: }

// CHECK: class VirtualRetainRelease {
// CHECK:   func doRetainVirtual()
// CHECK:   func doReleaseVirtual()
// CHECK: }
// CHECK: class DerivedVirtualRetainRelease : VirtualRetainRelease {
// CHECK:   func doRetainVirtual()
// CHECK:   func doReleaseVirtual()
// CHECK: }

// CHECK: class PureVirtualRetainRelease {
// CHECK:   func doRetainPure()
// CHECK:   func doReleasePure()
// CHECK: }
// CHECK: class DerivedPureVirtualRetainRelease : PureVirtualRetainRelease {
// CHECK:   func doRetainPure()
// CHECK:   func doReleasePure()
// CHECK:   var refCount: CInt
// CHECK: }

// UNSAFE: class RefCountedBox {
// UNSAFE: {{^}}  @unsafe(always) final func doRetain()
// UNSAFE: {{^}}  @unsafe(always) final func doRelease()
// UNSAFE: class DerivedRefCountedBox : RefCountedBox {
// UNSAFE: {{^}}  final func __synthesized_lifetimeAccessor_doRetain()
// UNSAFE: {{^}}  final func __synthesized_lifetimeAccessor_doRelease()
// UNSAFE: class TemplatedDerivedHasRelease<CInt> {
// UNSAFE: {{^}}  @unsafe(always) final func doReleaseTemplated()
// UNSAFE: {{^}}  @_addressableSelf @unsafe(always) func doRetainInBase()
// UNSAFE: class VirtualRetainRelease {
// UNSAFE: {{^}}  @unsafe(always) final func doRetainVirtual()
// UNSAFE: {{^}}  @unsafe(always) final func doReleaseVirtual()
// UNSAFE: class DerivedVirtualRetainRelease : VirtualRetainRelease {
// UNSAFE: {{^}}  @unsafe(always) final override func doRetainVirtual()
// UNSAFE: {{^}}  @unsafe(always) final override func doReleaseVirtual()
// UNSAFE: class PureVirtualRetainRelease {
// UNSAFE: {{^}}  @unsafe(always) final func doRetainPure()
// UNSAFE: {{^}}  @unsafe(always) final func doReleasePure()
// UNSAFE: class DerivedPureVirtualRetainRelease : PureVirtualRetainRelease {
// UNSAFE: {{^}}  @unsafe(always) final override func doRetainPure()
// UNSAFE: {{^}}  @unsafe(always) final override func doReleasePure()
// UNSAFE: class AmbiguousReleaseMethods {
// UNSAFE: {{^}}  @unsafe(always) final func doRetain()
// UNSAFE: {{^}}  @unsafe(always) final func doRelease()
// UNSAFE: {{^}}  final func doRelease(_ argument: CInt)
// UNSAFE: {{^}}@unsafe(always) func retainAmbiguousFreeReleaseAndRetainMethods(_ v: AmbiguousFreeReleaseAndRetainMethods!)
// UNSAFE: {{^}}func retainAmbiguousFreeReleaseAndRetainMethods()
// UNSAFE: {{^}}@unsafe(always) func releaseAmbiguousFreeReleaseAndRetainMethods(_ v: AmbiguousFreeReleaseAndRetainMethods!)
// UNSAFE: {{^}}func releaseAmbiguousFreeReleaseAndRetainMethods(_ v: AmbiguousFreeReleaseAndRetainMethods!, _ argument: CInt)
// UNSAFE: {{^}}@unsafe(always) func retainOnlyViaFunctionBox(_ v: OnlyViaFunctionBox!)
// UNSAFE: {{^}}@unsafe(always) func releaseOnlyViaFunctionBox(_ v: OnlyViaFunctionBox!)
// UNSAFE: class BaseOfReannotated {
// UNSAFE: {{^}}  @unsafe(always) final func baseRetain()
// UNSAFE: {{^}}  @unsafe(always) final func baseRelease()
// UNSAFE: class ImmortalDerivedFromShared {
// UNSAFE: {{^}}  @unsafe(always) final func baseRetain()
// UNSAFE: {{^}}  @unsafe(always) final func baseRelease()
// UNSAFE: {{^}}@unsafe class UnsafeDerivedFromShared {
// UNSAFE: {{^}}  @unsafe(always) final func baseRetain()
// UNSAFE: {{^}}  @unsafe(always) final func baseRelease()
// UNSAFE: class OwnOpsDerivedFromShared {
// UNSAFE: {{^}}  @unsafe(always) final func baseRetain()
// UNSAFE: {{^}}  @unsafe(always) final func baseRelease()
// UNSAFE: {{^}}  @unsafe(always) final func ownRetain()
// UNSAFE: {{^}}  @unsafe(always) final func ownRelease()
// UNSAFE: class VirtualOps {
// UNSAFE: {{^}}  @unsafe(always) final func virtualRetain()
// UNSAFE: {{^}}  @unsafe(always) final func virtualRelease()
// UNSAFE: {{^}}  final func unrelatedVirtual() -> CInt
// UNSAFE: {{^}}  @unsafe(always) final func explicitlyUnsafeVirtual() -> CInt
// UNSAFE: class OverridesVirtualOps : VirtualOps {
// UNSAFE: {{^}}  @unsafe(always) final override func virtualRetain()
// UNSAFE: {{^}}  @unsafe(always) final override func virtualRelease()
// UNSAFE: {{^}}  final override func unrelatedVirtual() -> CInt
// UNSAFE: {{^}}  final override func explicitlyUnsafeVirtual() -> CInt
// UNSAFE: class OverridesSkippingMiddle : DoesNotOverrideVirtualOps {
// UNSAFE: {{^}}  @unsafe(always) final override func virtualRetain()
// UNSAFE: {{^}}  @unsafe(always) final override func virtualRelease()
// UNSAFE: class OverridesWithNonPrimaryFRTBase {
// UNSAFE: {{^}}  @unsafe(always) final func virtualRetain()
// UNSAFE: {{^}}  @unsafe(always) final func virtualRelease()
// UNSAFE: {{^}}  final func unrelatedVirtual() -> CInt
// UNSAFE: class OverridesInBothBases : VirtualOps {
// UNSAFE: {{^}}  @unsafe(always) final override func virtualRetain()
// UNSAFE: {{^}}  @unsafe(always) final override func virtualRelease()
// UNSAFE: class OverridesThroughVirtualBase {
// UNSAFE: {{^}}  @unsafe(always) final func virtualRetain()
// UNSAFE: {{^}}  @unsafe(always) final func virtualRelease()
// UNSAFE: {{^}}  final func unrelatedVirtual() -> CInt
// UNSAFE: class TemplatedOverride<CInt> : VirtualOps {
// UNSAFE: {{^}}  @unsafe(always) final override func virtualRetain()
// UNSAFE: {{^}}  @unsafe(always) final override func virtualRelease()
// UNSAFE: class ReannotatedOverride {
// UNSAFE: {{^}}  final func virtualRetain()
// UNSAFE: {{^}}  final func virtualRelease()
// UNSAFE: {{^}}  final func unrelatedVirtual() -> CInt
// UNSAFE: {{^}}  @unsafe(always) final func ownRetain()
// UNSAFE: {{^}}  @unsafe(always) final func ownRelease()
// UNSAFE: class ImmortalReannotatedOverride {
// UNSAFE: {{^}}  final func virtualRetain()
// UNSAFE: {{^}}  final func virtualRelease()
// UNSAFE: class OverridesSwiftNamedOps : SwiftNamedVirtualOps {
// UNSAFE: {{^}}  @unsafe(always) final override func swiftRetain()
// UNSAFE: {{^}}  @unsafe(always) final override func swiftRelease()
// UNSAFE: class OnlyDerivedNamed : OnlyDerivedNamedBase {
// UNSAFE: {{^}}  @unsafe(always) final override func onlyRetain()
// UNSAFE: {{^}}  @unsafe(always) final override func onlyRelease()
// UNSAFE: class DiamondBottom {
// UNSAFE: {{^}}  @unsafe(always) final func virtualRetain()
// UNSAFE: {{^}}  @unsafe(always) final func virtualRelease()
// UNSAFE: class NonVirtualOps {
// UNSAFE: {{^}}  @unsafe(always) final func plainRetain()
// UNSAFE: {{^}}  @unsafe(always) final func plainRelease()
// UNSAFE: class HidesNonVirtualOps : NonVirtualOps {
// UNSAFE: {{^}}  final override func plainRetain()
// UNSAFE: {{^}}  final override func plainRelease()
// UNSAFE: class HidesNonVirtualOpsWithVirtual {
// UNSAFE: {{^}}  final func plainRetain()
// UNSAFE: {{^}}  final func plainRelease()
// UNSAFE: {{^}}class OverridesVirtualHidingNonVirtualOps : HidesNonVirtualOpsWithVirtual {
// UNSAFE: {{^}}  final override func plainRetain()
// UNSAFE: {{^}}  final override func plainRelease()
// UNSAFE: {{^}}class OverridesOnlyOtherBase {
// UNSAFE: {{^}}  final func plainRetain()
// UNSAFE: {{^}}  @unsafe(always) final func plainRelease()
