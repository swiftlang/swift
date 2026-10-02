// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-ir -enable-experimental-feature ImportCStructsWithArcFields %s | %FileCheck %s

// REQUIRES: objc_interop
// REQUIRES: swift_feature_ImportCStructsWithArcFields

import Foundation
import objc_structs

// Verify that imported ARC structs get non-trivial value witness tables with
// appropriate retain/release, and that these witnesses are correctly invoked
// through a generic context.
//
// Strong-only structs remain loadable with member-wise VWT.
// Weak-containing structs are address-only with Clang-synthesized VWT.

// --- VWT tables use non-trivial witnesses ---
// Strong-only: still uses member-wise VWT and __swift_memcpy for take.
// CHECK-DAG: @"$sSo19StrongsInAStructArcVWV" = {{.*}} %swift.vwtable { ptr @"$sSo19StrongsInAStructArcVwCP", ptr @"$sSo19StrongsInAStructArcVwxx", ptr @"$sSo19StrongsInAStructArcVwcp", ptr @"$sSo19StrongsInAStructArcVwca", ptr @__swift_memcpy

// --- Generic context calls through the VWT ---

// A generic function should load and call the VWT's initializeWithCopy.
// CHECK-LABEL: define hidden swiftcc void @"$s{{.*}}11genericCopyyxxlF"
// CHECK: %T.valueWitnesses = load ptr, ptr
// CHECK: %InitializeWithCopy = load ptr, ptr
// CHECK: call ptr %InitializeWithCopy(
// CHECK: ret void
func genericCopy<T>(_ x: T) -> T {
  return x
}

// When genericCopy is called with a strong ARC struct, the VWT's
// initializeWithCopy is invoked — exercising the member-wise retain path.
// CHECK-LABEL: define hidden swiftcc void @"$s{{.*}}21exerciseStrongGenericyyF"
// CHECK: call swiftcc void @"$s{{.*}}11genericCopyyxxlF"
func exerciseStrongGeneric() {
  let s = StrongsInAStructArc(myobj: MYObject())
  let copy = genericCopy(s)
  _ = copy
}

// Same for weak — VWT's initializeWithCopy calls the Clang copy constructor.
// CHECK-LABEL: define hidden swiftcc void @"$s{{.*}}19exerciseWeakGenericyyF"
// CHECK: call swiftcc void @"$s{{.*}}11genericCopyyxxlF"
func exerciseWeakGeneric() {
  let s = WeaksInAStructArc(myobj: MYObject())
  let copy = genericCopy(s)
  _ = copy
}

// Mixed struct through generic context.
// CHECK-LABEL: define hidden swiftcc void @"$s{{.*}}20exerciseMixedGenericyyF"
// CHECK: call swiftcc void @"$s{{.*}}11genericCopyyxxlF"
func exerciseMixedGeneric() {
  let s = MixedStrongWeakArc(strong: MYObject(), weak: nil, tag: 0)
  let copy = genericCopy(s)
  _ = copy
}

// --- Value witness function bodies ---

// Mixed destroy: calls Clang-synthesized destructor.
// CHECK-LABEL: define linkonce_odr hidden void @"$sSo18MixedStrongWeakArcVwxx"
// CHECK: call void @__destructor_8_s0_w8
// CHECK: ret void

// Mixed initializeWithCopy: calls Clang-synthesized copy constructor (may be
// inlined by Clang codegen into direct objc_retain + objc_copyWeak calls).
// CHECK-LABEL: define linkonce_odr hidden ptr @"$sSo18MixedStrongWeakArcVwcp"
// CHECK: @llvm.objc.retain

// Mixed assignWithCopy: calls Clang-synthesized copy assignment operator.
// CHECK-LABEL: define linkonce_odr hidden ptr @"$sSo18MixedStrongWeakArcVwca"
// CHECK: call void @__copy_assignment_8_8_s0_w8
// CHECK: ret ptr

// Weak destroy uses Clang-synthesized destructor.
// CHECK-LABEL: define linkonce_odr hidden void @"$sSo17WeaksInAStructArcVwxx"
// CHECK: call void @__destructor_8_w0
// CHECK: ret void

// Weak initializeWithCopy: Clang copy constructor.
// CHECK-LABEL: define linkonce_odr hidden ptr @"$sSo17WeaksInAStructArcVwcp"
// CHECK: call void @__copy_constructor_8_8_w0
// CHECK: ret ptr

// Strong destroy releases via objc_release.
// CHECK-LABEL: define linkonce_odr hidden void @"$sSo19StrongsInAStructArcVwxx"
// CHECK: call void @llvm.objc.release
// CHECK: ret void

// Strong initializeWithCopy retains via objc_retain.
// CHECK-LABEL: define linkonce_odr hidden ptr @"$sSo19StrongsInAStructArcVwcp"
// CHECK: call ptr @llvm.objc.retain
// CHECK: ret ptr

// Strong assignWithCopy retains the new value and releases the old.
// CHECK-LABEL: define linkonce_odr hidden ptr @"$sSo19StrongsInAStructArcVwca"
// CHECK: call ptr @llvm.objc.retain
// CHECK: call void @llvm.objc.release
// CHECK: ret ptr
