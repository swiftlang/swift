// No shipped runtime understands Transferable, so casts that involve a suppressed
// conformance to it are gated at every deployment target, including ones that
// already satisfy the Copyable and Escapable gates.

// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx12.0 -enable-experimental-feature NontransferableTypes -enable-experimental-feature NoncopyableCasting -verify-additional-prefix below60-
// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx15.0 -enable-experimental-feature NontransferableTypes -enable-experimental-feature NoncopyableCasting

// REQUIRES: swift_feature_NontransferableTypes
// REQUIRES: swift_feature_NoncopyableCasting
// REQUIRES: OS=macosx

struct NC: ~Copyable {}
struct NT: ~Transferable {}
struct NCNT: ~Copyable, ~Transferable {}

protocol PT: ~Escapable, ~Transferable {}
struct NTConforming: PT, ~Transferable {}

/// A phantom generic, so `Tag<T>` itself is Copyable, Escapable, and
/// Transferable no matter what its argument suppresses.
struct Tag<T: ~Copyable & ~Escapable & ~Transferable> { var n = 0 }

// A generic target keeps Sema from folding these statically.

func noncopyableArgument<T>(_ x: Tag<NC>, _: T.Type) -> Bool {
  // expected-below60-note @-1 {{add '@available' attribute to enclosing global function}}
  return x is T
  // expected-below60-error @-1 {{runtime support for casting types with non-'Copyable' generic arguments is only available in macOS 15.0.0 or newer}}
  // expected-below60-note @-2 {{add 'if #available' version check}}
}

func nontransferableArgument<T>(_ x: Tag<NT>, _: T.Type) -> Bool {
  // expected-note @-1 {{add '@available' attribute to enclosing global function}}
  return x is T
  // expected-error @-1 {{runtime support for casting types with non-transferable generic arguments is only available in}}
  // expected-note @-2 {{add 'if #available' version check}}
}

// The Transferable gate is the strictest one, so it's the one diagnosed.
func noncopyableNontransferableArgument<T>(_ x: Tag<NCNT>, _: T.Type) -> Bool {
  // expected-note @-1 {{add '@available' attribute to enclosing global function}}
  return x is T
  // expected-error @-1 {{runtime support for casting types with non-transferable generic arguments is only available in}}
  // expected-note @-2 {{add 'if #available' version check}}
}

func nontransferableExistential(_ x: any PT & ~Transferable) -> Bool {
  // expected-note @-1 {{add '@available' attribute to enclosing global function}}
  return x is NTConforming
  // expected-error @-1 {{runtime support for casting a non-'Transferable' existential is only available in}}
  // expected-note @-2 {{add 'if #available' version check}}
}

func nonescapableNontransferableExistential(_ x: consuming any PT & ~Escapable & ~Transferable) -> Bool {
  // expected-note @-1 {{add '@available' attribute to enclosing global function}}
  return x is NTConforming
  // expected-error @-1 {{runtime support for casting a non-'Transferable' existential is only available in}}
  // expected-note @-2 {{add 'if #available' version check}}
}

// An existential that only names a protocol suppressing Transferable is still
// Transferable.
func transferableExistential<T>(_ x: any PT, _: T.Type) -> Bool { x is T }
