// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -sil-verify-all \
// RUN:   -module-name test \
// RUN:   -disable-experimental-parser-round-trip \
// RUN:   -enable-experimental-feature NonescapableTypes \
// RUN:   -Xllvm -enable-lifetime-dependence-diagnostics \
// RUN:   2>&1 | %FileCheck %s

// REQUIRES: swift_in_compiler

@_nonescapable
struct BV {
  let p: UnsafeRawPointer
  let c: Int
}

func bv_copy(_ bv: borrowing BV) -> _copy(bv) BV {
  copy bv
}

// Diagnostics resolves mark_dependence [nonescaping].
//
// CHECK-LABEL: sil hidden @$s4test14bv_borrow_copy0B0AA2BVVAE_tF : $@convention(thin) (@guaranteed BV) -> _scope(1) @owned BV {
// CHECK: bb0(%0 : @noImplicitCopy $BV):
// CHECK:   [[R:%.*]] = apply %{{.*}}(%0) : $@convention(thin) (@guaranteed BV) -> _inherit(1) @owned BV
// CHECK:   [[MD:%.*]] = mark_dependence [nonescaping] [[R]] : $BV on %0 : $BV
// CHECK:   return [[MD]] : $BV
// CHECK-LABEL: } // end sil function '$s4test14bv_borrow_copy0B0AA2BVVAE_tF'
func bv_borrow_copy(bv: borrowing BV) -> _borrow(bv) BV {
  bv_copy(bv) 
}
