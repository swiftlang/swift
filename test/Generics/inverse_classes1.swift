// RUN: %target-typecheck-verify-swift \
// RUN:   -parse-stdlib -module-name Swift \
// RUN:   -enable-experimental-feature MoveOnlyClasses

// NOTE: -parse-stdlib is a transitional workaround and should not be required.

@_moveOnly
class KlassLegacy {}

class KlassModern: ~Copyable {}

class Konditional<T: ~Copyable> {}

func checks<T: ~Copyable, C>(
          _ a: KlassLegacy, // expected-error {{parameter of noncopyable type 'KlassLegacy' must specify ownership}} // expected-note 3{{add}}
          _ b: KlassModern, // expected-error {{parameter of noncopyable type 'KlassModern' must specify ownership}} // expected-note 3{{add}}
          _ c: Konditional<T>,
          _ d: Konditional<C>) {}
