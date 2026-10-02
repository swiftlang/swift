// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-silgen -enable-experimental-feature ImportCStructsWithArcFields %s | %FileCheck %s

// REQUIRES: objc_interop
// REQUIRES: swift_feature_ImportCStructsWithArcFields

import Foundation
import objc_structs

// A weak-field ARC struct should be address-only:
// @in_guaranteed for parameters, @out for returns, copy_addr for copies,
// struct_element_addr + load_weak for field access.

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}testCreateWeak
// CHECK: alloc_box ${ var WeaksInAStructArc }
// CHECK: apply {{%[0-9]+}}({{%[0-9]+}}, {{%[0-9]+}}) :
// CHECK-SAME: -> @out WeaksInAStructArc
// CHECK: destroy_addr
// CHECK: } // end sil function
func testCreateWeak() {
  var s = WeaksInAStructArc()
  _ = s
}

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}testCopyWeak
// CHECK: bb0(%0 : $*WeaksInAStructArc, %1 : $*WeaksInAStructArc):
// CHECK: copy_addr %1 to [init] %0
// CHECK: } // end sil function
func testCopyWeak(_ s: WeaksInAStructArc) -> WeaksInAStructArc {
  return s
}

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}testWeakFieldAccess
// CHECK: struct_element_addr %0, #WeaksInAStructArc.myobj
// CHECK: load_weak
// CHECK: } // end sil function
func testWeakFieldAccess(_ s: WeaksInAStructArc) -> MYObject? {
  return s.myobj
}
