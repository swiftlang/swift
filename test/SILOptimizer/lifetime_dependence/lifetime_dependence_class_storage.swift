// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -sil-verify-all \
// RUN:   -module-name test \
// RUN:   -disable-availability-checking \
// RUN:   -enable-experimental-feature Lifetimes

// Check which value each dependence is rooted at. A chain of immutable strong
// links must reach the reference at the root; a chain that contains a mutable
// or non-strong link must stop at that link instead.
//
// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -module-name test \
// RUN:   -disable-availability-checking \
// RUN:   -enable-experimental-feature Lifetimes \
// RUN:   -Xllvm -sil-print-after=lifetime-dependence-insertion 2>&1 | %FileCheck %s

// REQUIRES: swift_feature_Lifetimes

// A class instance holds its stored properties at a stable address for as long
// as the instance is alive. A borrow of an immutable one is therefore valid for
// as long as the reference is, so a dependence on it can be attributed to the
// reference -- transitively, through a chain of such references.

final class Leaf {
  let d = [1, 2, 3]
}

final class Mid { let c = Leaf() }
final class Top { let b = Mid() }

// MARK: - Accepted

final class OneLevel {
  let array = [1, 2, 3, 4, 5]

  // A single hop through a `let` stored property of a class.
  //
  // CHECK-LABEL: sil hidden [ossa] @$s4test8OneLevelC7getDatas4SpanVySiGyF : $@convention(method) (@guaranteed OneLevel) -> @lifetime(borrow 0) @owned Span<Int> {
  // CHECK: bb0(%0 : @guaranteed $OneLevel):
  // CHECK: [[SPAN:%.*]] = apply %{{.*}} : $@convention(method) <τ_0_0> (@guaranteed Array<τ_0_0>) -> @lifetime(borrow 0) @owned Span<τ_0_0>
  // CHECK: mark_dependence [unresolved] [[SPAN]] on %0
  // CHECK-LABEL: } // end sil function '$s4test8OneLevelC7getDatas4SpanVySiGyF'
  @_lifetime(borrow self)
  func getData() -> Span<Int> {
    return array.span
  }
}

final class Chained {
  let a = Top()

  // A chain of `let` stored properties of classes. Each link keeps the next
  // alive, so the buffer outlives any caller that keeps `self` alive. The
  // dependence must skip past the intermediate references, which are
  // temporaries destroyed as soon as the next one has been loaded, and root
  // itself in `self`.
  //
  // CHECK-LABEL: sil hidden [ossa] @$s4test7ChainedC7getDatas4SpanVySiGyF : $@convention(method) (@guaranteed Chained) -> @lifetime(borrow 0) @owned Span<Int> {
  // CHECK: bb0(%0 : @guaranteed $Chained):
  // CHECK: ref_element_addr %0, #Chained.a
  // CHECK: [[SPAN:%.*]] = apply %{{.*}} : $@convention(method) <τ_0_0> (@guaranteed Array<τ_0_0>) -> @lifetime(borrow 0) @owned Span<τ_0_0>
  // CHECK: mark_dependence [unresolved] [[SPAN]] on %0
  // CHECK-LABEL: } // end sil function '$s4test7ChainedC7getDatas4SpanVySiGyF'
  @_lifetime(borrow self)
  func getData() -> Span<Int> {
    return a.b.c.d.span
  }
}

struct InnerStruct { let d = [1, 2, 3] }
struct MidStruct { let c = InnerStruct() }

final class ClassThenStructs {
  let s = MidStruct()

  // Struct projections within the class's storage do not change the access
  // base, so this is still a single link.
  //
  // CHECK-LABEL: sil hidden [ossa] @$s4test16ClassThenStructsC7getDatas4SpanVySiGyF : $@convention(method) (@guaranteed ClassThenStructs) -> @lifetime(borrow 0) @owned Span<Int> {
  // CHECK: bb0(%0 : @guaranteed $ClassThenStructs):
  // CHECK: [[SPAN:%.*]] = apply %{{.*}} : $@convention(method) <τ_0_0> (@guaranteed Array<τ_0_0>) -> @lifetime(borrow 0) @owned Span<τ_0_0>
  // CHECK: mark_dependence [unresolved] [[SPAN]] on %0
  // CHECK-LABEL: } // end sil function '$s4test16ClassThenStructsC7getDatas4SpanVySiGyF'
  @_lifetime(borrow self)
  func getData() -> Span<Int> {
    return s.c.d.span
  }
}

struct MidVarStruct { var c = Mid() }

final class StructVarInLetField {
  let s = MidVarStruct()

  // `c` is a `var`, but it is reached through a `let` stored property, so it is
  // immutable all the same.
  //
  // CHECK-LABEL: sil hidden [ossa] @$s4test19StructVarInLetFieldC7getDatas4SpanVySiGyF : $@convention(method) (@guaranteed StructVarInLetField) -> @lifetime(borrow 0) @owned Span<Int> {
  // CHECK: bb0(%0 : @guaranteed $StructVarInLetField):
  // CHECK: [[SPAN:%.*]] = apply %{{.*}} : $@convention(method) <τ_0_0> (@guaranteed Array<τ_0_0>) -> @lifetime(borrow 0) @owned Span<τ_0_0>
  // CHECK: mark_dependence [unresolved] [[SPAN]] on %0
  // CHECK-LABEL: } // end sil function '$s4test19StructVarInLetFieldC7getDatas4SpanVySiGyF'
  @_lifetime(borrow self)
  func getData() -> Span<Int> {
    return s.c.c.d.span
  }
}

// A `let` on a non-final class: stored properties cannot be overridden, so the
// storage is still reached directly.
class Base { let d = [1, 2, 3] }

// CHECK-LABEL: sil hidden [ossa] @$s4test17fromNonFinalClassys4SpanVySiGAA4BaseCF : $@convention(thin) (@guaranteed Base) -> @lifetime(borrow 0) @owned Span<Int> {
// CHECK: bb0(%0 : @guaranteed $Base):
// CHECK: [[SPAN:%.*]] = apply %{{.*}} : $@convention(method) <τ_0_0> (@guaranteed Array<τ_0_0>) -> @lifetime(borrow 0) @owned Span<τ_0_0>
// CHECK: mark_dependence [unresolved] [[SPAN]] on %0
// CHECK-LABEL: } // end sil function '$s4test17fromNonFinalClassys4SpanVySiGAA4BaseCF'
@_lifetime(borrow b)
func fromNonFinalClass(_ b: Base) -> Span<Int> {
  return b.d.span
}

// A chain rooted at a parameter rather than at self.
//
// CHECK-LABEL: sil hidden [ossa] @$s4test13fromParameterys4SpanVySiGAA3TopCF : $@convention(thin) (@guaranteed Top) -> @lifetime(borrow 0) @owned Span<Int> {
// CHECK: bb0(%0 : @guaranteed $Top):
// CHECK: [[SPAN:%.*]] = apply %{{.*}} : $@convention(method) <τ_0_0> (@guaranteed Array<τ_0_0>) -> @lifetime(borrow 0) @owned Span<τ_0_0>
// CHECK: mark_dependence [unresolved] [[SPAN]] on %0
// CHECK-LABEL: } // end sil function '$s4test13fromParameterys4SpanVySiGAA3TopCF'
@_lifetime(borrow t)
func fromParameter(_ t: Top) -> Span<Int> {
  return t.b.c.d.span
}

// A chain rooted at a local: the dependence is scoped to the local's lifetime,
// which covers uses within this function. The root is the local's borrow, not
// the raw allocation result -- that is consumed by the move_value below, so a
// dependence on it would be outside its OSSA lifetime.
//
// CHECK-LABEL: sil hidden [ossa] @$s4test8localUseSiyF : $@convention(thin) () -> Int {
// CHECK: [[T:%.*]] = move_value [lexical] [var_decl] %{{.*}}
// CHECK: [[BORROW:%.*]] = begin_borrow [[T]]
// CHECK: ref_element_addr [[BORROW]], #Top.b
// CHECK: [[SPAN:%.*]] = apply %{{.*}} : $@convention(method) <τ_0_0> (@guaranteed Array<τ_0_0>) -> @lifetime(borrow 0) @owned Span<τ_0_0>
// CHECK: mark_dependence [unresolved] [[SPAN]] on [[BORROW]]
// CHECK-LABEL: } // end sil function '$s4test8localUseSiyF'
func localUse() -> Int {
  let t = Top()
  let span = t.b.c.d.span
  return span.count
}

// A class reference reached by `struct_extract` from a borrowed struct rather
// than loaded out of memory. Nothing may overwrite the struct's storage while
// it is borrowed, so the referent stays alive for the whole borrow. The
// dependence roots at the borrowed struct, not at the copy of the reference
// that SILGen makes, which is destroyed as soon as the next link is loaded.
struct StructThenClass {
  let mid: Mid

  // CHECK-LABEL: sil hidden [ossa] @$s4test15StructThenClassV7getDatas4SpanVySiGyF : $@convention(method) (@guaranteed StructThenClass) -> @lifetime(borrow 0) @owned Span<Int> {
  // CHECK: bb0(%0 : @guaranteed $StructThenClass):
  // CHECK: struct_extract %0, #StructThenClass.mid
  // CHECK: [[SPAN:%.*]] = apply %{{.*}} : $@convention(method) <τ_0_0> (@guaranteed Array<τ_0_0>) -> @lifetime(borrow 0) @owned Span<τ_0_0>
  // CHECK: mark_dependence [unresolved] [[SPAN]] on %0
  // CHECK-LABEL: } // end sil function '$s4test15StructThenClassV7getDatas4SpanVySiGyF'
  @_lifetime(borrow self)
  func getData() -> Span<Int> {
    return mid.c.d.span
  }
}

// MARK: - Rejected

final class MutableField {
  // A `var` can be reassigned, releasing the buffer while a Span still points
  // into it, so the dependence is on the access scope rather than on `self`.
  var array = [1, 2, 3]

  // CHECK-LABEL: sil hidden [ossa] @$s4test12MutableFieldC7getDatas4SpanVySiGyF : $@convention(method) (@guaranteed MutableField) -> @lifetime(borrow 0) @owned Span<Int> {
  // CHECK: bb0(%0 : @guaranteed $MutableField):
  // CHECK: [[ADDR:%.*]] = ref_element_addr %0, #MutableField.array
  // CHECK: [[ACCESS:%.*]] = begin_access [read] [dynamic] [[ADDR]]
  // CHECK: [[SPAN:%.*]] = apply %{{.*}} : $@convention(method) <τ_0_0> (@guaranteed Array<τ_0_0>) -> @lifetime(borrow 0) @owned Span<τ_0_0>
  // CHECK: mark_dependence [unresolved] [[SPAN]] on [[ACCESS]]
  // CHECK-LABEL: } // end sil function '$s4test12MutableFieldC7getDatas4SpanVySiGyF'
  @_lifetime(borrow self)
  func getData() -> Span<Int> {
    return array.span // expected-error {{lifetime-dependent value escapes its scope}}
    // expected-note @-1 {{it depends on this scoped access to variable 'array'}}
    // expected-note @-2 {{this use causes the lifetime-dependent value to escape}}
  }
}

final class MutableMidField {
  var mid = Mid()

  // A `var` anywhere in the chain is enough to reject it: the walk stops at the
  // access scope for `mid` instead of continuing to `self`.
  //
  // CHECK-LABEL: sil hidden [ossa] @$s4test15MutableMidFieldC7getDatas4SpanVySiGyF : $@convention(method) (@guaranteed MutableMidField) -> @lifetime(borrow 0) @owned Span<Int> {
  // CHECK: bb0(%0 : @guaranteed $MutableMidField):
  // CHECK: [[ADDR:%.*]] = ref_element_addr %0, #MutableMidField.mid
  // CHECK: [[ACCESS:%.*]] = begin_access [read] [dynamic] [[ADDR]]
  // CHECK: [[SPAN:%.*]] = apply %{{.*}} : $@convention(method) <τ_0_0> (@guaranteed Array<τ_0_0>) -> @lifetime(borrow 0) @owned Span<τ_0_0>
  // CHECK: mark_dependence [unresolved] [[SPAN]] on [[ACCESS]]
  // CHECK-LABEL: } // end sil function '$s4test15MutableMidFieldC7getDatas4SpanVySiGyF'
  @_lifetime(borrow self)
  func getData() -> Span<Int> {
    return mid.c.d.span // expected-error {{lifetime-dependent value escapes its scope}}
    // expected-note @-1 {{it depends on this scoped access to variable 'mid'}}
    // expected-note @-2 {{this use causes the lifetime-dependent value to escape}}
  }
}

final class UnownedLink {
  unowned let leaf: Leaf // expected-note {{it depends on the lifetime of variable 'leaf'}}

  init(_ l: Leaf) { leaf = l }

  // `unowned` does not keep the referent alive, so self does not transitively
  // own the buffer. Climbing through this link would be a false promise: the
  // dependence stops at the unowned field's storage rather than reaching %0.
  //
  // CHECK-LABEL: sil hidden [ossa] @$s4test11UnownedLinkC7getDatas4SpanVySiGyF : $@convention(method) (@guaranteed UnownedLink) -> @lifetime(borrow 0) @owned Span<Int> {
  // CHECK: bb0(%0 : @guaranteed $UnownedLink):
  // CHECK: [[ADDR:%.*]] = ref_element_addr %0, #UnownedLink.leaf
  // CHECK: [[SPAN:%.*]] = apply %{{.*}} : $@convention(method) <τ_0_0> (@guaranteed Array<τ_0_0>) -> @lifetime(borrow 0) @owned Span<τ_0_0>
  // CHECK: mark_dependence [unresolved] [[SPAN]] on [[ADDR]]
  // CHECK-LABEL: } // end sil function '$s4test11UnownedLinkC7getDatas4SpanVySiGyF'
  @_lifetime(borrow self)
  func getData() -> Span<Int> {
    return leaf.d.span // expected-error {{lifetime-dependent value escapes its scope}}
    // expected-note @-1 {{this use causes the lifetime-dependent value to escape}}
  }
}

final class UnownedUnsafeLink {
  unowned(unsafe) let leaf: Leaf // expected-note {{it depends on the lifetime of variable 'leaf'}}

  init(_ l: Leaf) { leaf = l }

  // CHECK-LABEL: sil hidden [ossa] @$s4test17UnownedUnsafeLinkC7getDatas4SpanVySiGyF : $@convention(method) (@guaranteed UnownedUnsafeLink) -> @lifetime(borrow 0) @owned Span<Int> {
  // CHECK: bb0(%0 : @guaranteed $UnownedUnsafeLink):
  // CHECK: [[ADDR:%.*]] = ref_element_addr %0, #UnownedUnsafeLink.leaf
  // CHECK: [[SPAN:%.*]] = apply %{{.*}} : $@convention(method) <τ_0_0> (@guaranteed Array<τ_0_0>) -> @lifetime(borrow 0) @owned Span<τ_0_0>
  // CHECK: mark_dependence [unresolved] [[SPAN]] on [[ADDR]]
  // CHECK-LABEL: } // end sil function '$s4test17UnownedUnsafeLinkC7getDatas4SpanVySiGyF'
  @_lifetime(borrow self)
  func getData() -> Span<Int> {
    return leaf.d.span // expected-error {{lifetime-dependent value escapes its scope}}
    // expected-note @-1 {{this use causes the lifetime-dependent value to escape}}
  }
}

// A chain rooted at a local cannot be returned: the local's lifetime ends here,
// while the Span is used by the caller. The dependence is still rooted at the
// local's borrow, which is what makes the escape diagnosable.
//
// CHECK-LABEL: sil hidden [ossa] @$s4test15returnFromLocals4SpanVySiGyF : $@convention(thin) () -> @lifetime(immortal) @owned Span<Int> {
// CHECK: [[T:%.*]] = move_value [lexical] [var_decl] %{{.*}}
// CHECK: [[BORROW:%.*]] = begin_borrow [[T]]
// CHECK: [[SPAN:%.*]] = apply %{{.*}} : $@convention(method) <τ_0_0> (@guaranteed Array<τ_0_0>) -> @lifetime(borrow 0) @owned Span<τ_0_0>
// CHECK: mark_dependence [unresolved] [[SPAN]] on [[BORROW]]
// CHECK-LABEL: } // end sil function '$s4test15returnFromLocals4SpanVySiGyF'
@_lifetime(immortal)
func returnFromLocal() -> Span<Int> {
  let t = Top() // expected-note {{it depends on the lifetime of variable 't'}}
  return t.b.c.d.span // expected-error {{lifetime-dependent value escapes its scope}}
  // expected-note @-1 {{this use causes the lifetime-dependent value to escape}}
}
