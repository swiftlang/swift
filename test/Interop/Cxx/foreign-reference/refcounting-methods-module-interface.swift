// RUN: %target-swift-ide-test -print-module -cxx-interoperability-mode=upcoming-swift -module-to-print=RefCountingMethods -I %S/Inputs -source-filename=x | %FileCheck --check-prefixes=CHECK,CHECK-OFF %s
// RUN: %target-swift-ide-test -print-module -cxx-interoperability-mode=upcoming-swift -enable-experimental-feature ForeignReferenceTypeInheritance -module-to-print=RefCountingMethods -I %S/Inputs -source-filename=x | %FileCheck --check-prefixes=CHECK,CHECK-ON %s

// REQUIRES: swift_feature_ForeignReferenceTypeInheritance

// CHECK: class RefCountedBox {
// CHECK:   func doRetain()
// CHECK:   func doRelease()
// CHECK: }
// CHECK-OFF: class DerivedRefCountedBox {
// CHECK-OFF:   func doRetain()
// CHECK-OFF:   func doRelease()
// CHECK-OFF: }
// CHECK-ON: class DerivedRefCountedBox : RefCountedBox {
// CHECK-ON: }

// CHECK: class DerivedHasRelease {
// CHECK:   func doRetainInBase()
// CHECK:   func doRelease()
// CHECK: }

// CHECK: class TemplatedDerivedHasRelease<CFloat> {
// CHECK:   var value: Float
// CHECK:   func doReleaseTemplated()
// CHECK:   func doRetainInBase()
// CHECK: }
// CHECK: class TemplatedDerivedHasRelease<CInt> {
// CHECK:   var value: Int32
// CHECK:   func doReleaseTemplated()
// CHECK:   func doRetainInBase()
// CHECK: }

// CHECK-OFF: class CRTPDerived {
// CHECK-ON: class CRTPDerived : CRTPBase<CRTPDerived> {
// CHECK:   var value: Int32
// CHECK: }

// CHECK: class VirtualRetainRelease {
// CHECK:   func doRetainVirtual()
// CHECK:   func doReleaseVirtual()
// CHECK: }
// CHECK-OFF: class DerivedVirtualRetainRelease {
// CHECK-ON: class DerivedVirtualRetainRelease : VirtualRetainRelease {
// CHECK:   func doRetainVirtual()
// CHECK:   func doReleaseVirtual()
// CHECK: }

// CHECK: class PureVirtualRetainRelease {
// CHECK:   func doRetainPure()
// CHECK:   func doReleasePure()
// CHECK: }
// CHECK-OFF: class DerivedPureVirtualRetainRelease {
// CHECK-ON: class DerivedPureVirtualRetainRelease : PureVirtualRetainRelease {
// CHECK:   func doRetainPure()
// CHECK:   func doReleasePure()
// CHECK:   var refCount: Int32
// CHECK: }
