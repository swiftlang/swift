// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx13.0 -enable-experimental-feature NoncopyableCasting

// REQUIRES: swift_feature_NoncopyableCasting
// REQUIRES: OS=macosx

// Casting out of a non-'Copyable' existential needs runtime support that older
// runtimes lack: unwrapping the container by taking its payload rather than
// copying it, and -- for a type test -- answering without producing a value at
// all. An older runtime copies instead, which traps in the payload's copy value
// witness, so this has to be a compile-time error rather than a link failure or
// a crash.
//
// This file pins the diagnostic down by building for a deployment target older
// than the feature's availability. Every other test of this feature passes
// -target %target-future-triple for exactly this reason.

protocol P: ~Copyable {}

struct NC: ~Copyable, P { var t: Int }

// MARK: - Every cast form out of the existential is gated

func exprIs(_ box: borrowing any P & ~Copyable) -> Bool {
  // expected-note @-1 {{add '@available' attribute to enclosing global function}}
  return box is NC
  // expected-error @-1 {{runtime support for casting out of a non-'Copyable' existential is only available in}}
  // expected-note @-2 {{add 'if #available' version check}}
}

func exprAsOptional(_ box: consuming any P & ~Copyable) -> Int {
  // expected-note @-1 {{add '@available' attribute to enclosing global function}}
  if let n = box as? NC { return n.t }
  // expected-error @-1 {{runtime support for casting out of a non-'Copyable' existential is only available in}}
  // expected-note @-2 {{add 'if #available' version check}}
  return -1
}

func exprAsForced(_ box: consuming any P & ~Copyable) -> Int {
  // expected-note @-1 {{add '@available' attribute to enclosing global function}}
  return (box as! NC).t
  // expected-error @-1 {{runtime support for casting out of a non-'Copyable' existential is only available in}}
  // expected-note @-2 {{add 'if #available' version check}}
}

func patternIs(_ box: borrowing any P & ~Copyable) -> Int {
  // expected-note @-1 {{add '@available' attribute to enclosing global function}}
  switch box {
  case is NC: return 1
  // expected-error @-1 {{runtime support for casting out of a non-'Copyable' existential is only available in}}
  // expected-note @-2 {{add 'if #available' version check}}
  default: return -1
  }
}

func patternBinding(_ box: consuming any P & ~Copyable) -> Int {
  // expected-note @-1 {{add '@available' attribute to enclosing global function}}
  if case let n as NC = box { return n.t }
  // expected-error @-1 {{runtime support for casting out of a non-'Copyable' existential is only available in}}
  // expected-note @-2 {{add 'if #available' version check}}
  return -1
}

// MARK: - Opting in per declaration

// The gate is ordinary availability, so @available narrows it without having to
// disable checking for the whole file.
@available(macOS 99.99, *)
func availableEnough(_ box: borrowing any P & ~Copyable) -> Bool {
  return box is NC // no error
}

// MARK: - A copyable target needs no new runtime

// The gate asks whether the cast might have to move a non-'Copyable' payload out
// of the container, and a concrete 'Copyable' target cannot: a successful cast
// proves the payload was that type, so the copy an older runtime performs is
// legal, and a failed cast copies nothing, because tryCast() dispatches on the
// payload's metadata before touching its value. So these are allowed even here.

struct CopyablePayload: P { var t: Int }

func copyableTargetIs(_ box: borrowing any P & ~Copyable) -> Bool {
  return box is CopyablePayload // no error
}

func copyableTargetAsOptional(_ box: consuming any P & ~Copyable) -> Int {
  if let c = box as? CopyablePayload { return c.t } // no error
  return -1
}

func copyableTargetAsForced(_ box: consuming any P & ~Copyable) -> Int {
  return (box as! CopyablePayload).t // no error
}

func copyableTargetPatternIs(_ box: borrowing any P & ~Copyable) -> Int {
  switch box {
  case is CopyablePayload: return 1 // no error
  default: return -1
  }
}

func copyableTargetBinding(_ box: consuming any P & ~Copyable) -> Int {
  if case let c as CopyablePayload = box { return c.t } // no error
  return -1
}

// MARK: - An existential target stays gated

// Even a 'Copyable' existential target is gated, because a non-'Copyable'
// payload can inhabit one: `any Q & ~Copyable` accepts it outright, and
// 'AnyObject' accepts anything by boxing it in '__SwiftValue', which copies.
// Only pattern position is checked here; an existential target in expression
// position is rejected outright, so the gate never gets a look at it (see
// test/Sema/noncopyable_existential_is_targets.swift).

protocol R: ~Copyable {}

func existentialTargetPatternIs(_ box: borrowing any P & ~Copyable) -> Int {
  // expected-note @-1 {{add '@available' attribute to enclosing global function}}
  switch box {
  case is any R & ~Copyable: return 1
  // expected-error @-1 {{runtime support for casting out of a non-'Copyable' existential is only available in}}
  // expected-note @-2 {{add 'if #available' version check}}
  // Unrelated to availability: no type here is known to conform to both. The
  // cast is still emitted, so the gate still has to fire.
  // expected-warning @-5 {{cast from 'any P & ~Copyable' to unrelated type 'any R & ~Copyable' always fails}}
  default: return -1
  }
}

// MARK: - A copyable existential is unaffected

protocol Q {}
struct C: Q {}

func copyableExistential(_ box: any Q) -> Bool {
  return box is C // no error
}

// A `~Copyable` *protocol* whose existential is nonetheless copyable is also
// unaffected: what matters is the existential type, not the protocol's
// suppressed conformance.
struct CopyableConformer: P { var t: Int }

func copyableConformer(_ box: any P) -> Bool {
  return box is CopyableConformer // no error
}
