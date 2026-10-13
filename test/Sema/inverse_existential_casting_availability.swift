// Three deployment-target bands, because two different runtime features are
// involved and they became available at different versions.
//
// An existential that suppresses `Copyable` or `Escapable` is represented as an
// extended existential shape, and casting one hands the runtime that shape --
// which needs `swift_getExtendedExistentialTypeMetadata_unique`, introduced in
// 5.7 / macOS 13. Below that the symbol is absent and the program fails to
// launch, so this has to be a compile-time error.
//
// A type whose *generic argument* suppresses one has a separate, stricter gate:
// the runtime must check the suppressed conformance during the cast, which is
// 6.0 / macOS 15.
//
// So: below 13 both fire, 13 and 14 only the generic-argument one, 15 and above
// neither. Pinning all three bands is the point -- a single target cannot
// distinguish the two thresholds, and conflating them is exactly how this was
// first written.

// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx12.0 -enable-experimental-feature NoncopyableCasting -enable-experimental-feature Lifetimes -verify-additional-prefix pre57- -verify-additional-prefix below60-
// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx13.0 -enable-experimental-feature NoncopyableCasting -enable-experimental-feature Lifetimes -verify-additional-prefix below60-
// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx14.0 -enable-experimental-feature NoncopyableCasting -enable-experimental-feature Lifetimes -verify-additional-prefix below60-
// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx15.0 -enable-experimental-feature NoncopyableCasting -enable-experimental-feature Lifetimes

// REQUIRES: swift_feature_NoncopyableCasting
// REQUIRES: swift_feature_Lifetimes
// REQUIRES: OS=macosx

protocol P: ~Copyable, ~Escapable {}

struct NC: P, ~Copyable { var t = 1 }

struct NE: P, ~Escapable {
  var t: Int
  @_lifetime(immortal) init(_ t: Int) { self.t = t }
}

/// A phantom generic: no stored property, so `Tag<NC>` is itself `Copyable` even
/// though its argument is not. That is what lets it be cast at all while still
/// carrying an inverse, which exercises the generic-argument gate rather than the
/// existential one.
struct Tag<T: ~Copyable & ~Escapable> { var n = 0 }

// MARK: - the existential's own layout: gated at 5.7

// Deliberately a `~Escapable` existential rather than a `~Copyable` one. A
// `~Copyable` existential is additionally gated by `DynamicCastTest`, whose
// availability is FUTURE, so it is rejected at *every* target here and the two
// thresholds below could not be told apart through it. The existential layout is
// what is being checked, and `~Escapable` isolates it. The `~Copyable` spelling
// is covered by noncopyable_existential_casting_availability.swift.
func nonescapableExistential(_ x: consuming any P & ~Escapable) -> Bool {
  // expected-pre57-note @-1 {{add '@available' attribute to enclosing global function}}
  return x is NE
  // expected-pre57-error @-1 {{runtime support for casting a non-'Escapable' existential is only available in macOS 13.0.0 or newer}}
  // expected-pre57-note @-2 {{add 'if #available' version check}}
}

// MARK: - a suppressing generic argument: gated at 6.0

// A generic target keeps Sema from folding these statically, which would
// otherwise add "always fails" noise unrelated to availability.
func noncopyableArgument<T>(_ x: Tag<NC>, _: T.Type) -> Bool {
  // expected-below60-note @-1 {{add '@available' attribute to enclosing global function}}
  return x is T
  // expected-below60-error @-1 {{runtime support for casting types with noncopyable generic arguments is only available in macOS 15.0.0 or newer}}
  // expected-below60-note @-2 {{add 'if #available' version check}}
}

func nonescapableArgument<T>(_ x: Tag<NE>, _: T.Type) -> Bool {
  // expected-below60-note @-1 {{add '@available' attribute to enclosing global function}}
  return x is T
  // expected-below60-error @-1 {{runtime support for casting types with nonescapable generic arguments is only available in macOS 15.0.0 or newer}}
  // expected-below60-note @-2 {{add 'if #available' version check}}
}

// MARK: - the thresholds are genuinely different

// At 13.0 and 14.0 the existential above is accepted while these two are not,
// which is the whole reason for the middle band. If both gates were keyed on the
// same version, the `below60` expectations here would fail in those two runs for
// want of a matching diagnostic.

// MARK: - an availability check satisfies either gate

@available(macOS 15.0, *)
func guardedByAttribute<T>(_ x: consuming any P & ~Escapable, _ y: Tag<NC>,
                           _: T.Type) -> Bool {
  return x is NE && y is T
}

func guardedByVersionCheck<T>(_ x: consuming any P & ~Escapable, _ y: Tag<NC>,
                              _: T.Type) -> Bool {
  if #available(macOS 15.0, *) {
    return x is NE && y is T
  }
  return false
}

// MARK: - what must not be gated at all

/// A plain existential suppresses nothing, so it needs neither feature.
protocol Q {}
func plainExistential<T>(_ x: any Q, _: T.Type) -> Bool { x is T }

/// Nor does a generic argument that suppresses nothing.
struct Plain<T> { var n = 0 }
func plainArgument<T>(_ x: Plain<Int>, _: T.Type) -> Bool { x is T }

/// Nor an ordinary concrete cast.
func concrete<T>(_ x: Int, _: T.Type) -> Bool { x is T }
