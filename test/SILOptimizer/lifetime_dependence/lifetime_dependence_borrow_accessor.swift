// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -sil-verify-all \
// RUN:   -module-name test \
// RUN:   -disable-availability-checking \
// RUN:   -enable-experimental-feature Lifetimes \
// RUN:   -enable-experimental-feature BorrowAndMutateAccessors

// Check which value each dependence is rooted at.
//
// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -module-name test \
// RUN:   -disable-availability-checking \
// RUN:   -enable-experimental-feature Lifetimes \
// RUN:   -enable-experimental-feature BorrowAndMutateAccessors \
// RUN:   -Xllvm -sil-print-after=lifetime-dependence-insertion 2>&1 | %FileCheck %s

// REQUIRES: swift_feature_Lifetimes
// REQUIRES: swift_feature_BorrowAndMutateAccessors

struct Wrapped {
  var span: Span<Int> {
    @_lifetime(borrow self)
    get { fatalError() }
  }
}

final class Holder {
  let _wrapped: Wrapped

  init(wrapped: Wrapped) { self._wrapped = wrapped }

  var wrapped: Wrapped {
    borrow { return _wrapped }
  }
}

// `Wrapped` above is empty, so the accessor's result is trivial and has no
// borrow introducers at all. A non-trivial result takes a different path -- the
// introducer walk reaches the accessor's `self` argument on its own -- so both
// have to be covered.
final class RichHolder {
  let _data = [1, 2, 3]

  var data: [Int] {
    borrow { return _data }
  }
}

// MARK: - Accepted

// The base is a guaranteed argument, so the accessor's result is valid for the
// whole function and the dependence roots at the argument.
//
// CHECK-LABEL: sil hidden [ossa] @$s4test12fromArgumentys4SpanVySiGAA6HolderCF : $@convention(thin) (@guaranteed Holder) -> @lifetime(borrow 0) @owned Span<Int> {
// CHECK: bb0(%0 : @guaranteed $Holder):
// CHECK: [[WRAPPED:%.*]] = apply %{{.*}}(%0) : $@convention(method) (@guaranteed Holder) -> @guaranteed Wrapped
// CHECK: [[SPAN:%.*]] = apply %{{.*}}([[WRAPPED]]) : $@convention(method) (Wrapped) -> @lifetime(borrow 0) @owned Span<Int>
// CHECK: mark_dependence [unresolved] [[SPAN]] on %0
// CHECK-LABEL: } // end sil function '$s4test12fromArgumentys4SpanVySiGAA6HolderCF'
@_lifetime(borrow h)
func fromArgument(_ h: Holder) -> Span<Int> {
  return h.wrapped.span
}

// The reference reaches the accessor through a `struct_extract` of a borrowed
// struct. SILGen copies and borrows that reference just for the accessor call,
// so the dependence must root at the borrowed struct instead.
struct Outer {
  let holder: Holder

  // CHECK-LABEL: sil hidden [ossa] @$s4test5OuterV4spans4SpanVySiGvg : $@convention(method) (@guaranteed Outer) -> @lifetime(borrow 0) @owned Span<Int> {
  // CHECK: bb0(%0 : @guaranteed $Outer):
  // CHECK: struct_extract %0, #Outer.holder
  // CHECK: [[SPAN:%.*]] = apply %{{.*}} : $@convention(method) (Wrapped) -> @lifetime(borrow 0) @owned Span<Int>
  // CHECK: mark_dependence [unresolved] [[SPAN]] on %0
  // CHECK-LABEL: } // end sil function '$s4test5OuterV4spans4SpanVySiGvg'
  var span: Span<Int> {
    @_lifetime(borrow self)
    get {
      holder.wrapped.span
    }
  }
}

// A dependent value used within the function, rather than returned.
func localUse(_ h: Holder) -> Int {
  let span = h.wrapped.span
  return span.count
}

// The same two shapes with a non-trivial accessor result. These reach the
// accessor's `self` argument through the borrow introducer walk rather than
// through the trivial path, so they must root at the same place.
//
// CHECK-LABEL: sil hidden [ossa] @$s4test8richFromys4SpanVySiGAA10RichHolderCF : $@convention(thin) (@guaranteed RichHolder) -> @lifetime(borrow 0) @owned Span<Int> {
// CHECK: bb0(%0 : @guaranteed $RichHolder):
// CHECK: mark_dependence [unresolved] %{{.*}} on %0
// CHECK-LABEL: } // end sil function '$s4test8richFromys4SpanVySiGAA10RichHolderCF'
@_lifetime(borrow h)
func richFrom(_ h: RichHolder) -> Span<Int> {
  return h.data.span
}

struct RichOuter {
  let holder: RichHolder

  // CHECK-LABEL: sil hidden [ossa] @$s4test9RichOuterV4spans4SpanVySiGvg : $@convention(method) (@guaranteed RichOuter) -> @lifetime(borrow 0) @owned Span<Int> {
  // CHECK: bb0(%0 : @guaranteed $RichOuter):
  // CHECK: struct_extract %0, #RichOuter.holder
  // CHECK: mark_dependence [unresolved] %{{.*}} on %0
  // CHECK-LABEL: } // end sil function '$s4test9RichOuterV4spans4SpanVySiGvg'
  var span: Span<Int> {
    @_lifetime(borrow self)
    get {
      holder.data.span
    }
  }
}

// MARK: - Rejected

// The base is a local: the accessor's result is only valid while that local is,
// so the dependent value cannot outlive the function. `other` gives the result
// something to depend on, so the only error is the escape itself.
@_lifetime(borrow other)
func escapingLocal(_ other: Holder) -> Span<Int> {
  let local = Holder(wrapped: Wrapped())
  return local.wrapped.span // expected-error {{lifetime-dependent value escapes its scope}}
  // expected-note @-2 {{it depends on the lifetime of variable 'local'}}
  // expected-note @-2 {{this use causes the lifetime-dependent value to escape}}
}
