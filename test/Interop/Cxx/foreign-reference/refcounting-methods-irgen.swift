// RUN: %target-swift-emit-irgen %s -I %S/Inputs -cxx-interoperability-mode=upcoming-swift -Xcc -fignore-exceptions -target %target-swift-5.8-abi-triple | %FileCheck %s

import RefCountingMethods

public func useCRTPDeleting() -> Int32 {
  let a = CRTPDeletingDerived.create(123)!
  return a.value
}

// CHECK: define linkonce_odr {{.*}}void @{{_ZNK16CRTPDeletingBaseI19CRTPDeletingDerivedE11crtpReleaseEv|"\?crtpRelease\@\?\$CRTPDeletingBase\@UCRTPDeletingDerived\@\@\@\@QEBAXXZ"}}(
// CHECK:   call void @{{_ZN16CRTPDeletingBaseI19CRTPDeletingDerivedEdlEPv|"\?\?3\?\$CRTPDeletingBase\@UCRTPDeletingDerived\@\@\@\@SAXPEAX\@Z"}}(
// CHECK: define linkonce_odr {{.*}}void @{{_ZN16CRTPDeletingBaseI19CRTPDeletingDerivedEdlEPv|"\?\?3\?\$CRTPDeletingBase\@UCRTPDeletingDerived\@\@\@\@SAXPEAX\@Z"}}(
