// RUN: %target-typecheck-verify-swift -enable-experimental-feature CoroutineAccessors

// REQUIRES: swift_feature_CoroutineAccessors

// Companion to generalized_accessors.swift's ReadModifiable: with the
// CoroutineAccessors feature enabled, `_read`/`_modify` are represented
// internally as `yielding borrow`/`yielding mutate` (the two are synonyms),
// but diagnostics must still name the accessor using the spelling the user
// actually wrote, not the internal representation.

struct ReadModifiable {
  var readAndWillSet: Int {
    _read {}
    willSet {} // expected-error {{'willSet' cannot be provided together with a '_read' accessor}}
  }

  var readAndDidSet: Int {
    _read {}
    didSet {} // expected-error {{'didSet' cannot be provided together with a '_read' accessor}}
  }
}

struct ModifyModifiable {
  var _s = 0

  // `_modify` is declared before `get` so it -- not the getter -- is the
  // first non-observing accessor and the one named in the diagnostic.
  var modifyAndWillSet: Int {
    _modify { yield &_s }
    get { _s }
    willSet {} // expected-error {{'willSet' cannot be provided together with a '_modify' accessor}}
  }

  var modifyAndDidSet: Int {
    _modify { yield &_s }
    get { _s }
    didSet {} // expected-error {{'didSet' cannot be provided together with a '_modify' accessor}}
  }
}
