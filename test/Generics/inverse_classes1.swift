// RUN: %target-typecheck-verify-swift \
// RUN:   -enable-experimental-feature MoveOnlyClasses

// REQUIRES: swift_feature_MoveOnlyClasses

class KlassModern: ~Copyable {}

class Konditional<T: ~Copyable> {}

func checks<T: ~Copyable, C>(
          _ b: KlassModern, // expected-error {{parameter of noncopyable type 'KlassModern' must specify ownership}} // expected-note 3{{add}}
          _ c: Konditional<T>,
          _ d: Konditional<C>) {}

// Make sure that MoveOnlyClasses don't suppress more than ~Copyable.
do {
  class NiceTry: ~Copyable, Copyable {} // expected-error {{class 'NiceTry' required to be 'Copyable' but is marked with '~Copyable'}}

  class KlassNonescapable: ~Escapable {} // expected-error {{classes cannot be '~Escapable'}}
  class KlassNonescapableButEscapable: ~Escapable, Escapable {} // expected-error {{classes cannot be '~Escapable'}}

  func requiresEscapable<T>(_: T.Type) {}
  func checkEscapable() {
    requiresEscapable(KlassNonescapable.self)
  }
}
