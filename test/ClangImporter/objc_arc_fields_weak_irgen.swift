// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-ir -enable-experimental-feature ImportCStructsWithArcFields %s | %FileCheck %s

// REQUIRES: objc_interop
// REQUIRES: swift_feature_ImportCStructsWithArcFields

import Foundation
import objc_structs

// IRGen should emit swift_unknownObjectWeakCopyInit for copies and
// swift_unknownObjectWeakDestroy for destroys of weak-field ARC structs.

// CHECK-LABEL: define {{.*}} @"$s{{.*}}testCreateAndDestroyWeak
// CHECK: call ptr @"$sSo17WeaksInAStructArcVWOh"
// CHECK: ret void
func testCreateAndDestroyWeak() {
  var s = WeaksInAStructArc()
  _ = s
}

// The copy outline should use swift_unknownObjectWeakCopyInit.
// CHECK-LABEL: define {{.*}} @"$sSo17WeaksInAStructArcVWOc"
// CHECK: call ptr @swift_unknownObjectWeakCopyInit
// CHECK: ret ptr

// The destroy outline should use swift_unknownObjectWeakDestroy.
// CHECK-LABEL: define {{.*}} @"$sSo17WeaksInAStructArcVWOh"
// CHECK: call void @swift_unknownObjectWeakDestroy
// CHECK: ret ptr

// CHECK-LABEL: define {{.*}} @"$s{{.*}}testCopyWeak
// CHECK: call ptr @"$sSo17WeaksInAStructArcVWOc"
// CHECK: ret void
func testCopyWeak(_ s: WeaksInAStructArc) -> WeaksInAStructArc {
  return s
}

// CHECK-LABEL: define {{.*}} @"$s{{.*}}testWeakFieldAccess
// CHECK: call ptr @swift_unknownObjectWeakLoadStrong
// CHECK: ret i64
func testWeakFieldAccess(_ s: WeaksInAStructArc) -> MYObject? {
  return s.myobj
}

// CHECK-LABEL: define {{.*}} @"$s{{.*}}testCreateAndDestroyMixed
// CHECK: call ptr @"$sSo18MixedStrongWeakArcVWOh"
// CHECK: ret void
func testCreateAndDestroyMixed() {
  var s = MixedStrongWeakArc(strong: MYObject(), weak: nil, tag: 0)
  _ = s
}

// Mixed struct destroy outline should release the strong field and
// weakDestroy the weak field.
// CHECK-LABEL: define {{.*}} @"$sSo18MixedStrongWeakArcVWOh"
// CHECK: call void @llvm.objc.release
// CHECK: call void @swift_unknownObjectWeakDestroy
// CHECK: ret ptr

// CHECK-LABEL: define {{.*}} @"$s{{.*}}testCopyMixed
// CHECK: call ptr @"$sSo18MixedStrongWeakArcVWOc"
// CHECK: ret void
func testCopyMixed(_ s: MixedStrongWeakArc) -> MixedStrongWeakArc {
  return s
}

// Mixed struct copy outline should retain the strong field and
// weakCopyInit the weak field.
// CHECK-LABEL: define {{.*}} @"$sSo18MixedStrongWeakArcVWOc"
// CHECK: call ptr @llvm.objc.retain
// CHECK: call ptr @swift_unknownObjectWeakCopyInit
// CHECK: ret ptr
