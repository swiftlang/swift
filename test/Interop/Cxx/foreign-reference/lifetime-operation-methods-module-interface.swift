// RUN: %target-swift-ide-test -print-module -cxx-interoperability-mode=upcoming-swift -I %swift_src_root/lib/ClangImporter/SwiftBridging -module-to-print=LifetimeOperationMethods -I %S/Inputs -source-filename=x | %FileCheck %s

// CHECK: class RefCountedBox {
// CHECK:   func doRetain() 
// CHECK:   func doRelease()
// CHECK: }
// CHECK: class DerivedRefCountedBox {
// CHECK:   func doRetain()
// CHECK:   func doRelease()
// CHECK: }

// CHECK: class DerivedHasRelease {
// CHECK:   func doRelease()
// CHECK:   func doRetainInBase()
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

// CHECK: class CRTPDerived {
// CHECK:   var value: Int32
// CHECK: }

// CHECK: class VirtualRetainRelease {
// CHECK:   func doRetainVirtual()
// CHECK:   func doReleaseVirtual()
// CHECK: }
// CHECK: class DerivedVirtualRetainRelease {
// CHECK:   func doRetainVirtual()
// CHECK:   func doReleaseVirtual()
// CHECK: }

// CHECK: class PureVirtualRetainRelease {
// CHECK:   func doRetainPure()
// CHECK:   func doReleasePure()
// CHECK: }
// CHECK: class DerivedPureVirtualRetainRelease {
// CHECK:   func doRetainPure()
// CHECK:   func doReleasePure()
// CHECK:   var refCount: Int32
// CHECK: }
