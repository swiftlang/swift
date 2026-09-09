// RUN: %target-swift-emit-silgen %s -target %target-future-triple -verify -enable-experimental-feature NoncopyableCasting

// REQUIRES: swift_feature_NoncopyableCasting

// Cast-pattern shapes over a noncopyable existential that SILGen does not
// implement yet. Each must produce an ordinary diagnostic: the failure mode for
// getting these wrong is a SILGen assert, i.e. a compiler crash, not a missed
// error. Both cases below were exactly that.
//
// These live apart from test/SILOptimizer/moveonly_noncopyable_existential_is.swift
// because a SILGen-phase error aborts before the mandatory passes run, which
// would suppress the move-only checker diagnostics that file relies on.

protocol P: ~Copyable {}

struct Big: ~Copyable, P { var tag: Int }
struct Unrelated: ~Copyable, P {}

// MARK: - Binding the payload
//
// Used to assert in bindBorrow(): Pattern::getOwnership leaves a cast pattern at
// Shared, so the switch runs as a borrow, but the ordinary lowering hands back a
// value whose consumption kind that borrow path rejects. Binding needs a
// borrowed projection out of the container, which is not implemented.

func switchWithLetBinding(_ box: consuming any P & ~Copyable) -> Int {
  switch box {
  case let b as Big: return b.tag
  // expected-error @-1 {{binding the payload of a non-'Copyable' value with a cast pattern is not implemented; use 'case is' to test the type without binding}}
  default: return -1
  }
}

func switchWithVarBinding(_ box: consuming any P & ~Copyable) -> Int {
  switch box {
  case var b as Big: b.tag += 1; return b.tag
  // expected-error @-1 {{binding the payload of a non-'Copyable' value with a cast pattern is not implemented; use 'case is' to test the type without binding}}
  default: return -1
  }
}

func switchWithBindingAmongTests(_ box: consuming any P & ~Copyable) -> Int {
  switch box {
  case is Unrelated: return 0
  case let b as Big: return b.tag
  // expected-error @-1 {{binding the payload of a non-'Copyable' value with a cast pattern is not implemented; use 'case is' to test the type without binding}}
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
