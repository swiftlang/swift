// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-silgen -enable-experimental-feature ImportCStructsWithArcFields %s | %FileCheck %s

// REQUIRES: objc_interop
// REQUIRES: swift_feature_ImportCStructsWithArcFields

import Foundation
import objc_structs

// A strong-only ARC struct should be loadable and non-trivial:
// copy_value for copies, destroy_value for destroys, struct_extract
// for field access.

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}testCreateAndDestroy
// CHECK: apply {{%[0-9]+}}({{%[0-9]+}}, {{%[0-9]+}}) :
// CHECK-SAME: -> @owned StrongsInAStructArc
// CHECK: destroy_value
// CHECK: } // end sil function
func testCreateAndDestroy() {
  let s = StrongsInAStructArc(myobj: MYObject())
  _ = s
}

// The struct should be passed @guaranteed (loadable, not address-only).
// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}testCopy
// CHECK: bb0(%0 : @guaranteed $StrongsInAStructArc):
// CHECK: copy_value %0
// CHECK: } // end sil function
func testCopy(_ s: StrongsInAStructArc) -> StrongsInAStructArc {
  return s
}

// Field access should use struct_extract.
// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}testFieldAccess
// CHECK: struct_extract %0, #StrongsInAStructArc.myobj
// CHECK: } // end sil function
func testFieldAccess(_ s: StrongsInAStructArc) -> MYObject {
  return s.myobj
}

// Passing an ARC struct to a C function: the struct is borrowed at the call site.
// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}testPassToCFunction
// CHECK: [[FN:%[0-9]+]] = function_ref @$sSo19takeStrongArcStructyySo016StrongsInAStructC0VFTo : $@convention(c) (StrongsInAStructArc) -> ()
// CHECK: apply [[FN]]({{%[0-9]+}}) : $@convention(c) (StrongsInAStructArc) -> ()
// CHECK: } // end sil function
func testPassToCFunction() {
  let s = StrongsInAStructArc(myobj: MYObject())
  takeStrongArcStruct(s)
}

// Receiving an ARC struct from a C function: the return is @autoreleased.
// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}testReceiveFromCFunction
// CHECK: [[FN:%[0-9]+]] = function_ref @$sSo21returnStrongArcStructSo016StrongsInAStructC0VyFTo : $@convention(c) () -> @autoreleased StrongsInAStructArc
// CHECK: apply [[FN]]() : $@convention(c) () -> @autoreleased StrongsInAStructArc
// CHECK: } // end sil function
func testReceiveFromCFunction() -> MYObject {
  let s = returnStrongArcStruct()
  return s.myobj
}
