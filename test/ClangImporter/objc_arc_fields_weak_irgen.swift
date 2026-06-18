// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-ir -enable-experimental-feature ImportCStructsWithArcFields %s | %FileCheck %s

// REQUIRES: objc_interop
// REQUIRES: swift_feature_ImportCStructsWithArcFields

import Foundation
import objc_structs

// Structs with __weak fields are address-only. Their VWT operations use
// Clang-synthesized helper functions that call ObjC runtime weak operations
// (objc_copyWeak, objc_destroyWeak, etc.) directly.

// CHECK-LABEL: define {{.*}} @"$s{{.*}}testCreateAndDestroyWeak
// CHECK: alloca %TSo17WeaksInAStructArcV
// CHECK: call void @__destructor_8_w0
// CHECK: ret void
func testCreateAndDestroyWeak() {
  var s = WeaksInAStructArc()
  _ = s
}

// testCopyWeak is address-only: indirect sret return and indirect parameter.
// CHECK-LABEL: define {{.*}} @"$s{{.*}}testCopyWeak
// CHECK-SAME: ptr noalias sret(%TSo17WeaksInAStructArcV)
// CHECK-SAME: ptr noalias dereferenceable
// CHECK: call void @__copy_constructor_8_8_w0
// CHECK: ret void
func testCopyWeak(_ s: WeaksInAStructArc) -> WeaksInAStructArc {
  return s
}

// Field access still uses the existing swift_unknownObjectWeakLoadStrong
// runtime function, which correctly dispatches based on the object's
// nativeness at runtime.
// CHECK-LABEL: define {{.*}} @"$s{{.*}}testWeakFieldAccess
// CHECK: call ptr @swift_unknownObjectWeakLoadStrong
// CHECK: ret
func testWeakFieldAccess(_ s: WeaksInAStructArc) -> MYObject? {
  return s.myobj
}

// Mixed strong/weak structs are also address-only due to the weak field.
// CHECK-LABEL: define {{.*}} @"$s{{.*}}testCreateAndDestroyMixed
// CHECK: call void @__destructor_8_s0_w8
// CHECK: ret void
func testCreateAndDestroyMixed() {
  var s = MixedStrongWeakArc(strong: MYObject(), weak: nil, tag: 0)
  _ = s
}

// CHECK-LABEL: define {{.*}} @"$s{{.*}}testCopyMixed
// CHECK-SAME: ptr noalias sret(%TSo18MixedStrongWeakArcV)
// CHECK-SAME: ptr noalias dereferenceable
// CHECK: call void @__copy_constructor_8_8_s0_w8
// CHECK: ret void
func testCopyMixed(_ s: MixedStrongWeakArc) -> MixedStrongWeakArc {
  return s
}
