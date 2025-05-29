// RUN: %target-typecheck-verify-swift \
// RUN:   -enable-experimental-feature MoveOnlyClasses

// REQUIRES: swift_feature_MoveOnlyClasses

class KlassModern: ~Copyable {}

class Konditional<T: ~Copyable> {}

func checks<T: ~Copyable, C>(
          _ b: KlassModern, // expected-error {{parameter of noncopyable type 'KlassModern' must specify ownership}} // expected-note 3{{add}}
          _ c: Konditional<T>,
          _ d: Konditional<C>) {}
