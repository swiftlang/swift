// RUN: %target-swift-frontend %s -Xllvm -sil-print-types -emit-sil \
// RUN:   -sil-verify-all \
// RUN:   -module-name test \
// RUN:   -enable-experimental-feature Lifetimes \
// RUN:   2>&1 | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Lifetimes

struct BV : ~Escapable {
  let p: UnsafeRawPointer
  let c: Int
  @_lifetime(borrow p)
  init(_ p: UnsafeRawPointer, _ c: Int) {
    self.p = p
    self.c = c
  }
}

@_lifetime(copy bv)
func bv_copy(_ bv: borrowing BV) -> BV {
  copy bv
}

struct NCInt: ~Copyable {
  var i: Int
}

public struct NEInt: ~Escapable {
  var i: Int

  // Test yielding an address.
  // CHECK-LABEL: sil hidden @$s4test5NEIntV5iprop{{.*}}vM : $@yield_once @convention(method) (@inout NEInt) -> @lifetime(borrow 0) @yields @inout NEInt
  // CHECK: bb0(%0 : $*NEInt):
  // CHECK: [[A:%.*]] = begin_access [modify] [static] %0 : $*NEInt
  // CHECK: yield [[A]] : $*NEInt, resume bb1, unwind bb2
  // CHECK: end_access [[A]] : $*NEInt
  // CHECK: end_access [[A]] : $*NEInt
  var iprop: NEInt {
    @_lifetime(copy self)
    _read { yield self }
    @_lifetime(&self)
    _modify { yield &self }
  }

  @_lifetime(borrow owner)
  init(owner: borrowing NCInt) {
    self.i = owner.i
  }
}

public enum NEOptional<Wrapped: ~Escapable>: ~Escapable {
  case none
  case some(Wrapped)
}

extension NEOptional where Wrapped: ~Escapable {
  // Test that enum initialization passes diagnostics.
  @_lifetime(copy some)
  public init(_ some: consuming Wrapped) { self = .some(some) }
}

func takeClosure(_: () -> ()) {}

// No mark_dependence is needed for a inherited scope.
//
// CHECK-LABEL: sil hidden @$s4test14bv_borrow_copyyAA2BVVADF : $@convention(thin) (@guaranteed BV) -> @lifetime(borrow 0)  @owned BV {
// CHECK:      bb0(%0 : @noImplicitCopy $BV):
// CHECK:        apply %{{.*}}(%0) : $@convention(thin) (@guaranteed BV) -> @lifetime(copy 0) @owned BV
// CHECK-NEXT:   return %3 : $BV
// CHECK-LABEL: } // end sil function '$s4test14bv_borrow_copyyAA2BVVADF'
@_lifetime(borrow bv)
func bv_borrow_copy(_ bv: borrowing BV) -> BV {
  bv_copy(bv) 
}

// The mark_dependence for the borrow scope should be marked
// [nonescaping] after diagnostics.
//
// CHECK-LABEL: sil hidden @$s4test010bv_borrow_C00B0AA2BVVAE_tF : $@convention(thin) (@guaranteed BV) -> @lifetime(borrow 0)  @owned BV {
// CHECK:       bb0(%0 : @noImplicitCopy $BV):
// CHECK:         [[R:%.*]] = apply %{{.*}}(%0) : $@convention(thin) (@guaranteed BV) -> @lifetime(borrow 0) @owned BV
// CHECK:         %{{.*}} = mark_dependence [nonescaping] [[R]] : $BV on %0 : $BV
// CHECK-NEXT:    return %{{.*}} : $BV
// CHECK-LABEL: } // end sil function '$s4test010bv_borrow_C00B0AA2BVVAE_tF'
@_lifetime(borrow bv)
func bv_borrow_borrow(bv: borrowing BV) -> BV {
  bv_borrow_copy(bv)
}

// This already has a mark_dependence [nonescaping] before diagnostics. If it triggers diagnostics again, it will fail
// because lifetime dependence does not expect a dependence directly on an 'inout' address without any 'begin_access'
// marker.
func ncint_capture(ncInt: inout NCInt) {
  takeClosure { _ = ncInt.i }
}

func neint_throws(ncInt: borrowing NCInt) throws -> NEInt {
  return NEInt(owner: ncInt)
}

// CHECK-LABEL: sil hidden @$s4test9neint_try5ncIntAA5NEIntVAA5NCIntV_tKF : $@convention(thin) (@guaranteed NCInt) -> @lifetime(borrow 0)  (@owned NEInt, @error any Error) {
// CHECK:   try_apply %{{.*}}(%0) : $@convention(thin) (@guaranteed NCInt) -> @lifetime(borrow 0)  (@owned NEInt, @error any Error), normal bb1, error bb2
// CHECK: bb1([[R:%.*]] : $NEInt):
// CHECK:   [[MD:%.*]] = mark_dependence [nonescaping] %5 : $NEInt on %0 : $NCInt
// CHECK:   return [[MD]] : $NEInt
// CHECK: bb2([[E:%.*]] : $any Error):
// CHECK:   throw [[E]] : $any Error
// CHECK-LABEL: } // end sil function '$s4test9neint_try5ncIntAA5NEIntVAA5NCIntV_tKF'
func neint_try(ncInt: borrowing NCInt) throws -> NEInt {
  try neint_throws(ncInt: ncInt)
}
