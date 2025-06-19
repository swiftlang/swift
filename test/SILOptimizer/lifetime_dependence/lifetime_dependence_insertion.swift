// RUN: %target-swift-frontend %s -Xllvm -sil-print-types -emit-sil \
// RUN:   -Xllvm -sil-print-types -Xllvm -sil-print-after=lifetime-dependence-insertion \
// RUN:   -sil-verify-all \
// RUN:   -module-name test \
// RUN:   -enable-experimental-feature Lifetimes \
// RUN:   -o /dev/null 2>&1 | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Lifetimes

struct BV : ~Escapable {
  let p: UnsafeRawPointer
  let i: Int

  @_lifetime(borrow p)
  init(_ p: UnsafeRawPointer, _ i: Int) {
    self.p = p
    self.i = i
  }
}

struct NC : ~Copyable {
  let p: UnsafeRawPointer
  let i: Int

  // Requires a borrow.
  @_lifetime(borrow self)
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

// LifetimeDependence.Scope needs to see through typed-to-raw pointer conversion.
//
// CHECK-LABEL: sil hidden [ossa] @$s4test18bv_pointer_convert1pAA2BVVSPySiG_tF : $@convention(thin) (UnsafePointer<Int>) -> @lifetime(borrow 0) @owned BV {
// CHECK: bb0(%0 : $UnsafePointer<Int>):
// CHECK: apply %{{.*}}<UnsafePointer<Int>, UnsafeRawPointer>([[RAW:%.*]], %{{.*}}) : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : _Pointer, τ_0_1 : _Pointer> (@in_guaranteed τ_0_0) -> @out τ_0_1
// CHECK: [[RAW:%.*]] = load [trivial] %6 : $*UnsafeRawPointer
// CHECK: [[BV:%.*]] = apply %13([[RAW]], {{.*}}) : $@convention(method) (UnsafeRawPointer, Int, @thin BV.Type) -> @lifetime(borrow 0) @owned BV
// CHECK: [[MD:%.*]] = mark_dependence [unresolved] [[BV]] : $BV on %0 : $UnsafePointer<Int>
// CHECK: return [[MD]] : $BV
// CHECK-LABEL: } // end sil function '$s4test18bv_pointer_convert1pAA2BVVSPySiG_tF'
@_lifetime(borrow p)
func bv_pointer_convert(p: UnsafePointer<Int>) -> BV {
  BV(p, 0)
}
