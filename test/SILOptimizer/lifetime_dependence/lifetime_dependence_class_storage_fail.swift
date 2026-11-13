// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -module-name test \
// RUN:   -disable-availability-checking \
// RUN:   -enable-experimental-feature Lifetimes

// Check that the dependence stops at the first link that does not qualify,
// rather than climbing all the way to %0.
//
// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -module-name test \
// RUN:   -disable-availability-checking \
// RUN:   -enable-experimental-feature Lifetimes \
// RUN:   -Xllvm -sil-print-after=lifetime-dependence-insertion 2>&1 | %FileCheck %s

// REQUIRES: swift_feature_Lifetimes

// Companion to lifetime_dependence_class_storage.swift for the cases whose
// dependence is rooted in storage reached through a temporary class reference.
//
// FIXME: this file deliberately omits -sil-verify-all. Each case below is
// correctly rejected, but LifetimeDependenceInsertion roots the placeholder
// mark_dependence in storage owned by a temporary -- an intermediate reference
// that has already been destroyed by the time the dependent value is produced --
// so the SIL is transiently invalid until a later pass rewrites it. That is a
// pre-existing problem, not specific to class stored properties: before
// immutable class storage was understood as a dependence root, every one of
// these cases hit it, including the ones that are now accepted.

final class Leaf {
  let d = [1, 2, 3]
  var mutableD = [1, 2, 3]
}

final class Mid { let c = Leaf() }

final class UnownedLink {
  unowned let leaf: Leaf // expected-note {{it depends on the lifetime of variable 'leaf'}}
  init(_ l: Leaf) { leaf = l }
}

final class MutableLeafField {
  let mid = Mid()

  // A `var` at the end of a chain of `let` links: the dependence stops at the
  // access scope for `mutableD`.
  //
  // CHECK-LABEL: sil hidden [ossa] @$s4test16MutableLeafFieldC7getDatas4SpanVySiGyF : $@convention(method) (@guaranteed MutableLeafField) -> @lifetime(borrow 0) @owned Span<Int> {
  // CHECK: bb0(%0 : @guaranteed $MutableLeafField):
  // CHECK: [[LEAF:%.*]] = ref_element_addr %{{.*}}, #Leaf.mutableD
  // CHECK: [[ACCESS:%.*]] = begin_access [read] [dynamic] [[LEAF]]
  // CHECK: [[SPAN:%.*]] = apply %{{.*}} : $@convention(method) <τ_0_0> (@guaranteed Array<τ_0_0>) -> @lifetime(borrow 0) @owned Span<τ_0_0>
  // CHECK: mark_dependence [unresolved] [[SPAN]] on [[ACCESS]]
  // CHECK-LABEL: } // end sil function '$s4test16MutableLeafFieldC7getDatas4SpanVySiGyF'
  @_lifetime(borrow self)
  func getData() -> Span<Int> {
    return mid.c.mutableD.span // expected-error {{lifetime-dependent value escapes its scope}}
    // expected-note @-1 {{it depends on this scoped access to variable 'mutableD'}}
    // expected-note @-2 {{this use causes the lifetime-dependent value to escape}}
  }
}

final class UnownedMidLink {
  let holder: UnownedLink

  init(_ h: UnownedLink) { holder = h }

  // A non-strong link in the middle of an otherwise strong chain: the walk
  // stops at the unowned field's storage instead of continuing to %0.
  //
  // CHECK-LABEL: sil hidden [ossa] @$s4test14UnownedMidLinkC7getDatas4SpanVySiGyF : $@convention(method) (@guaranteed UnownedMidLink) -> @lifetime(borrow 0) @owned Span<Int> {
  // CHECK: bb0(%0 : @guaranteed $UnownedMidLink):
  // CHECK: ref_element_addr %0, #UnownedMidLink.holder
  // CHECK: [[UNOWNED:%.*]] = ref_element_addr %{{.*}}, #UnownedLink.leaf
  // CHECK: [[SPAN:%.*]] = apply %{{.*}} : $@convention(method) <τ_0_0> (@guaranteed Array<τ_0_0>) -> @lifetime(borrow 0) @owned Span<τ_0_0>
  // CHECK: mark_dependence [unresolved] [[SPAN]] on [[UNOWNED]]
  // CHECK-LABEL: } // end sil function '$s4test14UnownedMidLinkC7getDatas4SpanVySiGyF'
  @_lifetime(borrow self)
  func getData() -> Span<Int> {
    return holder.leaf.d.span // expected-error {{lifetime-dependent value escapes its scope}}
    // expected-note @-1 {{this use causes the lifetime-dependent value to escape}}
  }
}

final class WeakLink {
  weak let leaf: Leaf? // expected-note {{it depends on the lifetime of variable 'leaf'}}

  init(_ l: Leaf) { leaf = l }

  // `weak` does not keep the referent alive. The unwrapped reference arrives as
  // an owned block argument, so the dependence is rooted in that temporary's
  // borrow rather than in %0.
  //
  // CHECK-LABEL: sil hidden [ossa] @$s4test8WeakLinkC7getDatas4SpanVySiGyF : $@convention(method) (@guaranteed WeakLink) -> @lifetime(borrow 0) @owned Span<Int> {
  // CHECK: bb0(%0 : @guaranteed $WeakLink):
  // CHECK: [[ADDR:%.*]] = ref_element_addr %0, #WeakLink.leaf
  // CHECK: load_weak [[ADDR]]
  // CHECK: bb2([[UNWRAPPED:%.*]] : @owned $Leaf):
  // CHECK: [[BORROW:%.*]] = begin_borrow [[UNWRAPPED]]
  // CHECK: ref_element_addr [[BORROW]], #Leaf.d
  // CHECK: [[SPAN:%.*]] = apply %{{.*}} : $@convention(method) <τ_0_0> (@guaranteed Array<τ_0_0>) -> @lifetime(borrow 0) @owned Span<τ_0_0>
  // CHECK: mark_dependence [unresolved] [[SPAN]] on [[BORROW]]
  // CHECK-LABEL: } // end sil function '$s4test8WeakLinkC7getDatas4SpanVySiGyF'
  @_lifetime(borrow self)
  func getData() -> Span<Int> {
    return leaf!.d.span // expected-error {{lifetime-dependent value escapes its scope}}
    // expected-note @-1 {{this use causes the lifetime-dependent value to escape}}
  }
}
