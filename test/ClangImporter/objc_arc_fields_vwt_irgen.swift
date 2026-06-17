// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-ir -enable-experimental-feature ImportCStructsWithArcFields %s | %FileCheck %s

// REQUIRES: objc_interop
// REQUIRES: swift_feature_ImportCStructsWithArcFields

import Foundation
import objc_structs

// Verify that imported ARC structs get non-trivial value witness tables with
// member-wise retain/release, and that these witnesses are correctly invoked
// through a generic context.

// --- VWT tables use non-trivial witnesses for copy/destroy, memcpy for take ---
// CHECK-DAG: @"$sSo19StrongsInAStructArcVWV" = {{.*}} %swift.vwtable { ptr @"$sSo19StrongsInAStructArcVwCP", ptr @"$sSo19StrongsInAStructArcVwxx", ptr @"$sSo19StrongsInAStructArcVwcp", ptr @"$sSo19StrongsInAStructArcVwca", ptr @__swift_memcpy
// CHECK-DAG: @"$sSo17WeaksInAStructArcVWV" = {{.*}} %swift.vwtable { ptr @"$sSo17WeaksInAStructArcVwCP", ptr @"$sSo17WeaksInAStructArcVwxx", ptr @"$sSo17WeaksInAStructArcVwcp", ptr @"$sSo17WeaksInAStructArcVwca", ptr @__swift_memcpy
// CHECK-DAG: @"$sSo18MixedStrongWeakArcVWV" = {{.*}} %swift.vwtable { ptr @"$sSo18MixedStrongWeakArcVwCP", ptr @"$sSo18MixedStrongWeakArcVwxx", ptr @"$sSo18MixedStrongWeakArcVwcp", ptr @"$sSo18MixedStrongWeakArcVwca", ptr @__swift_memcpy

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

// Same for weak — VWT's initializeWithCopy calls weakCopyInit.
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
// VWT functions are emitted per-type in VWT field order (destroy, initWithCopy,
// assignWithCopy). Types emit in order: Mixed, Weak, Strong.

// Mixed destroy: releases strong field, destroys weak field.
// CHECK-LABEL: define linkonce_odr hidden void @"$sSo18MixedStrongWeakArcVwxx"
// CHECK: call void @llvm.objc.release
// CHECK: call void @swift_unknownObjectWeakDestroy
// CHECK: ret void

// Mixed initializeWithCopy: retains strong field, copies weak field.
// CHECK-LABEL: define linkonce_odr hidden ptr @"$sSo18MixedStrongWeakArcVwcp"
// CHECK: call ptr @llvm.objc.retain
// CHECK: call ptr @swift_unknownObjectWeakCopyInit
// CHECK: ret ptr

// Mixed assignWithCopy: retains new strong, releases old strong, copies weak.
// CHECK-LABEL: define linkonce_odr hidden ptr @"$sSo18MixedStrongWeakArcVwca"
// CHECK: call ptr @llvm.objc.retain
// CHECK: call void @llvm.objc.release
// CHECK: call ptr @swift_unknownObjectWeakCopyAssign
// CHECK: ret ptr

// Weak destroy uses swift_unknownObjectWeakDestroy.
// CHECK-LABEL: define linkonce_odr hidden void @"$sSo17WeaksInAStructArcVwxx"
// CHECK: call void @swift_unknownObjectWeakDestroy
// CHECK: ret void

// Weak initializeWithCopy uses swift_unknownObjectWeakCopyInit.
// CHECK-LABEL: define linkonce_odr hidden ptr @"$sSo17WeaksInAStructArcVwcp"
// CHECK: call ptr @swift_unknownObjectWeakCopyInit
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
