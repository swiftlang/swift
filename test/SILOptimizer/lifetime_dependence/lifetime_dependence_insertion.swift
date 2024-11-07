// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -Xllvm -sil-print-after=lifetime-dependence-insertion \
// RUN:   -sil-verify-all \
// RUN:   -module-name test \
// RUN:   -enable-experimental-feature NonescapableTypes \
// RUN:   -disable-experimental-parser-round-trip \
// RUN:   -o /dev/null 2>&1 | %FileCheck %s
// FIXME: Remove '-disable-experimental-parser-round-trip' (rdar://137636751).

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_NonescapableTypes

struct BV : ~Escapable {
  let p: UnsafeRawPointer
  let i: Int

  @lifetime(borrow p)
  init(_ p: UnsafeRawPointer, _ i: Int) {
    self.p = p
    self.i = i
  }
}

struct NC : ~Copyable {
  let p: UnsafeRawPointer
  let i: Int

  // Requires a borrow.
  @lifetime(borrow self)
  borrowing func getBV() -> BV {
    BV(p, i)
  }
}

@_silgen_name("use")
func use(_ o : borrowing BV)

// CHECK-LABEL: sil hidden [ossa] @$s4test13bv_borrow_var1p1iySV_SitF : $@convention(thin) (UnsafeRawPointer, Int) -> () {
// CHECK: [[A:%.*]] = begin_access [read] [unknown] %{{.*}} : $*NC
// CHECK: [[U:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[A]] : $*NC 
// CHECK: [[L:%.*]] = load [copy] [[U]] : $*NC
// CHECK:   [[R:%.*]] = apply %{{.*}}([[L]]) : $@convention(method) (@guaranteed NC) -> @lifetime(borrow 0) @owned BV
// CHECK:   [[M:%.*]] = mark_dependence [unresolved] [[R]] : $BV on [[A]] : $*NC
// CHECK:   end_access [[A]] : $*NC
// CHECK:   [[MV:%.*]] = move_value [var_decl] [[M]] : $BV
// CHECK:   %{{.*}} = apply %{{.*}}([[MV]]) : $@convention(thin) (@guaranteed BV) -> ()
// CHECK-LABEL: } // end sil function '$s4test13bv_borrow_var1p1iySV_SitF'
func bv_borrow_var(p: UnsafeRawPointer, i: Int) {
  var nc = NC(p: p, i: i)
  let bv = nc.getBV()
  use(bv)
}
