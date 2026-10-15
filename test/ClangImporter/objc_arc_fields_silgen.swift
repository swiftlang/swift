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

// Receiving a non-trivial ARC struct from a C function produces an owned,
// loadable SIL value when Clang returns the record directly.
// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}testReceiveFromCFunction
// CHECK: [[FN:%[0-9]+]] = function_ref @$sSo21returnStrongArcStructSo016StrongsInAStructC0VyFTo : $@convention(c) () -> @owned StrongsInAStructArc
// CHECK: apply [[FN]]() : $@convention(c) () -> @owned StrongsInAStructArc
// CHECK: } // end sil function
func testReceiveFromCFunction() -> MYObject {
  let s = returnStrongArcStruct()
  return s.myobj
}

// Record size does not change the direct, owned SIL convention.
// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}testReceiveStrongPairFromCFunction
// CHECK: [[PAIR_FN:%[0-9]+]] = function_ref {{.*}} : $@convention(c) () -> @owned StrongPairArc
// CHECK: apply [[PAIR_FN]]() : $@convention(c) () -> @owned StrongPairArc
// CHECK: } // end sil function
func testReceiveStrongPairFromCFunction() -> MYObject {
  return returnStrongPairArc().first
}

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}testReceiveLargeStrongStructFromCFunction
// CHECK: [[LARGE_FN:%[0-9]+]] = function_ref {{.*}} : $@convention(c) () -> @owned LargeStrongArcStruct
// CHECK: apply [[LARGE_FN]]() : $@convention(c) () -> @owned LargeStrongArcStruct
// CHECK: } // end sil function
func testReceiveLargeStrongStructFromCFunction() -> MYObject {
  return returnLargeStrongArcStruct().first
}

// Nested records inherit the non-trivial, owned result convention.
// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}testReceiveNestedStrongStructFromCFunction
// CHECK: [[NESTED_FN:%[0-9]+]] = function_ref {{.*}} : $@convention(c) () -> @owned OuterArcStruct
// CHECK: apply [[NESTED_FN]]() : $@convention(c) () -> @owned OuterArcStruct
// CHECK: } // end sil function
func testReceiveNestedStrongStructFromCFunction() -> MYObject {
  return returnOuterArcStruct().nested.inner
}

// Imported C function pointer types use the same owned, loadable results.
// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}testReceiveStrongPairFromFunctionPointer
// CHECK: apply {{%[0-9]+}}() : $@convention(c) () -> @owned StrongPairArc
// CHECK: } // end sil function
func testReceiveStrongPairFromFunctionPointer(_ factory: StrongPairFactory) -> MYObject {
  return factory().first
}

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}testReceiveLargeStrongStructFromFunctionPointer
// CHECK: apply {{%[0-9]+}}() : $@convention(c) () -> @owned LargeStrongArcStruct
// CHECK: } // end sil function
func testReceiveLargeStrongStructFromFunctionPointer(_ factory: LargeStrongArcFactory) -> MYObject {
  return factory().first
}
