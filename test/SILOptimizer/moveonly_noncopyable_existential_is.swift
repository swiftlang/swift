// RUN: %target-swift-emit-sil %s -target %target-future-triple -sil-verify-all -verify -enable-experimental-feature NoncopyableCasting

// REQUIRES: swift_feature_NoncopyableCasting

// `is` and `case is T` on a noncopyable existential go through a non-consuming
// type test (`checked_cast_addr_br test_only`), so they impose no consumability
// requirement on their subject at all -- not a borrowing parameter, not a
// global, not a `let` stored property. This file pins down that reach, and the
// two nearby forms that are still consuming.
//
// Everything here is checked by the move-only address checker, which runs after
// SILGen. Shapes rejected in an earlier phase live in separate files; see the
// pointer block below for why they cannot share this one.

protocol P: ~Copyable {}
protocol Q: ~Copyable {}

struct Big: ~Copyable, P {
  var tag: Int
  var pad0, pad1, pad2, pad3, pad4, pad5: Int
}

struct Unrelated: ~Copyable, P {}

func mk(_ t: Int) -> Big {
  Big(tag: t, pad0: 0, pad1: 0, pad2: 0, pad3: 0, pad4: 0, pad5: 0)
}

// MARK: - `is` needs only a borrow, so every subject kind is allowed
//
// Note the contrast with cast *patterns that bind*, which need a consumable
// subject: see moveonly_noncopyable_existential_cast_patterns.swift, where a
// borrowing parameter and a global are both errors.

func isBorrowingParam(_ box: borrowing any P & ~Copyable) -> Bool {
  box is Big
}

func isInoutParam(_ box: inout any P & ~Copyable) -> Bool {
  box is Big
}

func isConsumingParam(_ box: consuming any P & ~Copyable) -> Bool {
  box is Big
}

let globalBox: any P & ~Copyable = mk(1)

func isGlobalLet() -> Bool { globalBox is Big }

enum Static {
  static let box: any P & ~Copyable = mk(2)
}

func isStaticLet() -> Bool { Static.box is Big }

struct LetHolder: ~Copyable {
  let inner: any P & ~Copyable
}

func isLetStoredProperty(_ h: borrowing LetHolder) -> Bool { h.inner is Big }

struct VarHolder: ~Copyable {
  var inner: any P & ~Copyable
}

func isVarStoredProperty(_ h: borrowing VarHolder) -> Bool { h.inner is Big }

func isRValueSubject() -> Bool { mk(3) as any P & ~Copyable is Big }

// Repeated tests on one subject: each used to consume it.
func isRepeated(_ box: borrowing any P & ~Copyable) -> Bool {
  (box is Big) && !(box is Unrelated) && (box is Big)
}

// Testing then consuming is fine: the test left the subject intact.
func isThenConsume(_ box: consuming any P & ~Copyable) -> Int {
  if box is Big {
    if case let b as Big = box { return b.tag }
  }
  return -1
}

// MARK: - `case is T` likewise

func switchBorrowing(_ box: borrowing any P & ~Copyable) -> Int {
  switch box {
  case is Big: return 1
  case is Unrelated: return 2
  default: return -1
  }
}

func switchGlobal() -> Int {
  switch globalBox {
  case is Big: return 1
  default: return -1
  }
}

// Nothing in the switch consumed the subject, so it is still usable after.
func subjectUsableAfterSwitch(_ box: consuming any P & ~Copyable) -> Int {
  switch box {
  case is Unrelated: break
  default: break
  }
  if case let b as Big = box { return b.tag }
  return -1
}

// `if`/`guard`/`while case is` share emitStmtCondition and take the same path.
// The `if` form used to assert in PossiblyUniquePtr.h.
func ifCaseIs(_ box: borrowing any P & ~Copyable) -> Int {
  if case is Big = box { return 1 }
  return -1
}

func guardCaseIs(_ box: borrowing any P & ~Copyable) -> Int {
  guard case is Big = box else { return -1 }
  return 1
}

func whileCaseIs(_ box: borrowing any P & ~Copyable) -> Int {
  while case is Big = box { return 1 }
  return -1
}

// MARK: - Shapes handled in other files
//
// Two groups are deliberately elsewhere, because an error in an earlier compiler
// phase aborts before the move-only checker runs and would suppress every
// diagnostic below:
//
//  * Binding the payload (`case let b as T`) and multi-pattern case labels are
//    SILGen-phase errors --
//    test/SILGen/noncopyable_existential_is_unimplemented.swift
//  * Existential and generic-parameter targets in *expression* position are
//    Sema-phase errors --
//    test/Sema/noncopyable_existential_is_targets.swift

// MARK: - An Optional target is not an `is` at all
//
// Sema desugars `box is Big?` into a conditional cast producing `Big??` and a
// nil check -- the value-producing `as?` path, which consumes its subject by
// design because there is no failure edge to leave it on. So it stays rejected
// on a borrowed subject even though the plain `is` form is fine.

func toOptionalTarget(_ box: borrowing any P & ~Copyable) -> Bool {
  // expected-error @-1 {{'box' is borrowed and cannot be consumed}}
  return box is Big?
  // expected-note @-1 {{consumed here}}
}

// MARK: - `as?` remains consuming

func asOptionalOnBorrowed(_ box: borrowing any P & ~Copyable) -> Int {
  // expected-error @-1 {{'box' is borrowed and cannot be consumed}}
  if let b = box as? Big { return b.tag }
  // expected-note @-1 {{consumed here}}
  return -1
}
