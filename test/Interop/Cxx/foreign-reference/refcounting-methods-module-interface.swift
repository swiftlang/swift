// RUN: %target-swift-ide-test -print-module -cxx-interoperability-mode=upcoming-swift -module-to-print=RefCountingMethods -I %S/Inputs -source-filename=x | %FileCheck %s

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
