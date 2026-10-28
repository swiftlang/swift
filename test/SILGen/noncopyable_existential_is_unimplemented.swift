// RUN: %target-swift-emit-silgen %s -target %target-future-triple -verify -enable-experimental-feature NoncopyableCasting

// REQUIRES: swift_feature_NoncopyableCasting

// Cast-pattern shapes over a non-`Copyable` subject that are not supported.
// Each must produce an ordinary diagnostic: the failure mode for getting these
// wrong is a SILGen assert, i.e. a compiler crash, not a missed error. Most of
// the cases below were exactly that.
//
// These live apart from test/SILOptimizer/moveonly_noncopyable_existential_is.swift
// because a SILGen-phase error aborts before the mandatory passes run, which
// would suppress the move-only checker diagnostics that file relies on.

protocol P: ~Copyable {}

struct Big: ~Copyable, P { var tag: Int }
struct Unrelated: ~Copyable, P {}
enum Shape: ~Copyable, P { case square(Int), circle }

// MARK: - Binding the payload
//
// Used to assert in bindBorrow(): Pattern::getOwnership leaves a cast pattern at
// Shared, so the switch runs as a borrow, but the ordinary lowering hands back a
// value whose consumption kind that borrow path rejects. Binding would need a
// borrowed projection out of the container. A `switch` without `consume` is a
// borrowing switch whatever the parameter's convention, so `consuming` alone
// doesn't help; `switch consume` does.

func switchWithLetBinding(_ box: consuming any P & ~Copyable) -> Int {
  switch box {
  case let b as Big: return b.tag
  // expected-error @-1 {{cannot bind the payload of a non-'Copyable' value with a cast pattern while the value is borrowed}}
  // expected-note @-2 {{use 'switch consume' to bind the payload by consuming the value}}
  default: return -1
  }
}

func switchWithVarBinding(_ box: consuming any P & ~Copyable) -> Int {
  switch box {
  case var b as Big: b.tag += 1; return b.tag
  // expected-error @-1 {{cannot bind the payload of a non-'Copyable' value with a cast pattern while the value is borrowed}}
  // expected-note @-2 {{use 'switch consume' to bind the payload by consuming the value}}
  default: return -1
  }
}

func switchWithBindingAmongTests(_ box: consuming any P & ~Copyable) -> Int {
  switch box {
  case is Unrelated: return 0
  case let b as Big: return b.tag
  // expected-error @-1 {{cannot bind the payload of a non-'Copyable' value with a cast pattern while the value is borrowed}}
  // expected-note @-2 {{use 'switch consume' to bind the payload by consuming the value}}
  default: return -1
  }
}

// MARK: - Binding under a `where` guard
//
// A consuming switch tests its cases under a borrow and binds the payload only
// once a case is chosen, but a `where` guard runs before that and would need the
// binding. Guarded cases are therefore not supported.

func consumingSwitchWithGuard(_ box: consuming any P & ~Copyable) -> Int {
  switch consume box {
  case let b as Big where b.tag > 0: return b.tag
  // expected-error @-1 {{cannot bind the payload of a non-'Copyable' value with a cast pattern while the value is borrowed}}
  // expected-note @-2 {{a 'where' clause is evaluated before the value is consumed; test the condition in the case body instead}}
  default: return -1
  }
}

// MARK: - Multiple patterns per case label, and fallthrough
//
// Rejected by the shared-case-block machinery, independent of this patch. These
// bind nothing, so they reach the type test and are rejected after it.

func multipleLabelItems(_ box: borrowing any P & ~Copyable) -> Int {
  switch box {
  case is Big, is Unrelated: return 1
  // expected-error @-1 {{matching a non-'Copyable' value using a case label that has multiple patterns is not implemented}}
  default: return -1
  }
}

func fallthroughBetweenCases(_ box: borrowing any P & ~Copyable) -> Int {
  switch box {
  case is Big: fallthrough
  case is Unrelated: return 1
  // expected-error @-1 {{matching a non-'Copyable' value using a case label that has multiple patterns is not implemented}}
  default: return -1
  }
}

// MARK: - Statically true binding casts whose binding can't be deferred
//
// A cast the subject's type already satisfies is a Coercion, so there is no
// test to perform. An identity cast binds the subject itself (see
// test/Interpreter/noncopyable_switch_statically_true_cast.swift). Erasure to
// an existential would need a borrowed existential view of the subject; when
// the binding is needed before the consuming phase -- a borrowing `switch`, or
// a `where` guard -- it is diagnosed like any other binding that can't wait.
// These used to crash SILGen (bindBorrow's BorrowAlways assertion).

func erasureInBorrowingSwitch(_ box: borrowing Big) -> Int {
  switch box {
  case let b as any P & ~Copyable: _ = b; return 1
  // expected-error @-1 {{cannot bind the payload of a non-'Copyable' value with a cast pattern while the value is borrowed}}
  // expected-note @-2 {{use 'switch consume' to bind the payload by consuming the value}}
  }
}

func erasureWithGuard(_ box: consuming Big, _ flag: Bool) -> Int {
  switch consume box {
  case let b as any P & ~Copyable where flag: _ = b; return 1
  // expected-error @-1 {{cannot bind the payload of a non-'Copyable' value with a cast pattern while the value is borrowed}}
  // expected-note @-2 {{a 'where' clause is evaluated before the value is consumed; test the condition in the case body instead}}
  default: return 0
  }
}

func erasureFromConsumingParameter(_ box: consuming Big) -> Int {
  switch box { // a borrowing switch: no `consume`
  case let b as any P & ~Copyable: _ = b; return 1
  // expected-error @-1 {{cannot bind the payload of a non-'Copyable' value with a cast pattern while the value is borrowed}}
  // expected-note @-2 {{use 'switch consume' to bind the payload by consuming the value}}
  }
}

// MARK: - A refutable sub-pattern
//
// The sub-pattern has to be matched against the payload during the borrowing
// phase, before the case is chosen and the payload taken.

func refutableSubpattern(_ box: consuming any P & ~Copyable) -> Int {
  switch consume box {
  case .square(let n) as Shape: return n
  // expected-error @-1 {{cannot bind the payload of a non-'Copyable' value with a cast pattern while the value is borrowed}}
  // expected-note @-2 {{match the bound value in the case body instead}}
  default: return 0
  }
}
