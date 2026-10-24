// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-ir -enable-experimental-feature ImportCStructsWithArcFields %s | %FileCheck %s

// REQUIRES: objc_interop
// REQUIRES: swift_feature_ImportCStructsWithArcFields

import Foundation
import objc_structs

// IRGen should emit objc_retain for copy and objc_release for destroy
// of strong-only ARC structs, using field-by-field value witnesses.

// CHECK-LABEL: define {{.*}} @"$s{{.*}}testCreateAndDestroy
// CHECK: call {{.*}} @"$sSo8MYObjectCABycfC"
// CHECK: call void @llvm.objc.release
// CHECK: ret void
func testCreateAndDestroy() {
  let s = StrongsInAStructArc(myobj: MYObject())
  _ = s
}

// CHECK-LABEL: define {{.*}} @"$s{{.*}}testCopy
// CHECK: call ptr @llvm.objc.retain
// CHECK: ret ptr
func testCopy(_ s: StrongsInAStructArc) -> StrongsInAStructArc {
  return s
}

// CHECK-LABEL: define {{.*}} @"$s{{.*}}testFieldAccess
// CHECK: call ptr @llvm.objc.retain
// CHECK: ret ptr
func testFieldAccess(_ s: StrongsInAStructArc) -> MYObject {
  return s.myobj
}
